{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import ClassyPrelude hiding (Map)
import Data.Maybe
import Data.Array
import XML
import WindwardopolisClient
import Types
import System.Exit
import Graph
import Data.List (minimumBy)
import Prelude (head)

import Control.Lens

main :: IO ()
main = do
  args <- getArgs
  let Just serverAddr = headMay args <|> Just "127.0.0.1"
  
  putStrLn$ "Connecting to "++serverAddr
  
  runClient (unpack serverAddr) "1707" $ \(send, get) -> do
    -- Join the game
    --send $ Join "Brainfuck" "ÜNICÖDE Y Ü NO CÓDE?!" "Harvard Med" 
    send $ Join "Brainfuck" "UNICODE Y U NO CODE?!" "Harvard Med" 

    stateVar <- newEmptyMVar
    -- intialize game
    SetupMessage game <- get Nothing
    putMVar stateVar game
    counterVar <- newMVar 0

    forever $ do
      -- read an update
      state <- readMVar stateVar
      message <-  get (Just state)
      case message of
        UpdateMessage update ->
          modifyMVar_ stateVar $ return . const (getGame update)
        SetupMessage game ->
          modifyMVar_ stateVar $ return . const game
        Exit -> do
          putStrLn "Received exit message"
          exitSuccess

      counter <- takeMVar counterVar
      let order = case counter `mod` 4 of
                    0 ->CardOrder PlayCard RelocateAllPassengers
                    1 ->CardOrder PlayCard RelocateAllCars
                    2 ->CardOrder DrawCard RelocateAllCars
                    3 ->CardOrder DrawCard RelocateAllPassengers
      send order
      putMVar counterVar (counter + 1)
      
      -- compute orders and send them
      orders <- doOrders <$> readMVar stateVar
      mapM send orders

doOrders :: Game -> [Command]
doOrders game =
  [Move [] []]


getGame :: GameUpdate -> Game
getGame (NoPathUpdate game) = game
getGame (UpdateUpdate game) = game
getGame (PassengerPickedUpUpdate game) = game
getGame (PassengerDeliveredUpdate game) = game
getGame (PassengerDeliveredAndPickedUpUpdate game) = game
getGame (PassengerRefusedEnemyUpdate game) = game
getGame (CoffeeStoreCarRestockedUpdate game) = game
getGame (PassengerAbandoned game) = game
getGame (PassengerNoActionUpdate game) = game
getGame (PowerupStatus game) = game

tileAt :: Map -> Location -> Tile
tileAt map loc = (map^.tiles) ! (loc^.x, loc^.y)

data Destination = Pass Passenger | Coffee Store

heuristic :: Game -> Location -> Destination -> Destination -> Int
heuristic game curLoc oneD twoD = 
  case (oneD, twoD) of
    {-
    (Coffee one, Coffee two) -> -100
    _ -> 100
    -}
    (Pass one, Pass two) ->
      let mp = game^.gameMap
          oneRoute = head $ one^.route
          twoRoute = head $ two^.route
          curTile = tileAt mp curLoc
          dToFirst = pathToDestination mp curTile (tileAt mp (one^.passengerLoc))
          firstToDest = pathToDestination mp (tileAt mp (one^.passengerLoc)) (tileAt mp (oneRoute^.companyLoc))
          firstDestToSecond = pathToDestination mp  (tileAt mp (oneRoute^.companyLoc)) (tileAt mp (two^.passengerLoc))
          secondToDest = pathToDestination mp (tileAt mp (two^.passengerLoc)) (tileAt mp (two^.passengerLoc)) in
        length dToFirst + length firstToDest + length firstDestToSecond + length secondToDest
    (Coffee one, Pass two) -> 
      case myCoffee of
        0 -> -100
        1 -> heurDest game curLoc (one^.storeLoc) (two^.passengerLoc)
        _ -> 10000000
    (Pass one, Coffee two) -> 
      case myCoffee of
        0 -> 1000000000
        _ -> heurDest game curLoc (one^.passengerLoc) (two^.storeLoc)
    (Coffee one, Coffee two) -> 10000000
  where
    Just self = getPlayerByGuid (game^.players) (game^.myGuid)
    myCoffee = fromMaybe 3 $ self^.coffees

    heurDest game curLoc loc1 loc2 = 
      let mp = game^.gameMap
          curTile = tileAt mp curLoc
          dToFirst = pathToDestination mp curTile (tileAt mp loc1)
          firstToSecond = pathToDestination mp (tileAt mp loc1) (tileAt mp loc2) in
        length dToFirst + length firstToSecond


findBestPair :: Game -> (Destination, Destination)
findBestPair game = bestPair
  where
    bestPair = fst $ minimumBy (compare `on` snd) $ zip allPairs scores 
    possibleDests = map Pass availablePassengers ++ map Coffee (game^.stores)
    allPairs = filter allowed $ map pairToTuple $ sequence [possibleDests, possibleDests]
    Just self = getPlayerByGuid (game^.players) (game^.myGuid)
    scores = map (uncurry $ heuristic game (self^.playerLoc)) allPairs
    allowed (Coffee x, Coffee y) = True
    allowed _ = False
    {-
    allowed (Pass x, Pass y) = (x ^. passengerName) /= (y ^. passengerName)
    allowed _ = True
    -}
    availablePassengers = filter isAvaliable (game ^. passengers)
    isAvaliable passenger =
      (passenger ^. passengerStatus) == Waiting &&
      not (enemyAtDestination game passenger)
    pairToTuple [a, b] = (a, b)

enemyAtDestination :: Game -> Passenger -> Bool
enemyAtDestination game p = case p^.route of
  []        -> False
  company:_ -> any (p `hasEnemy`) (passengersAt game company)

passengersAt :: Game -> Company -> [Passenger]
passengersAt game c = filter (\p -> p^.passengerLoc == c^.companyLoc) (game^.passengers)

hasEnemy :: Passenger -> Passenger -> Bool
hasEnemy p1 p2 = p2 `elem` (p1^.enemies)
