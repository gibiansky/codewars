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
import Data.List (maximumBy)
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
      
      -- compute orders and send them
      orders <- doOrders <$> readMVar stateVar
      mapM send orders

doOrders :: Game -> [Command]
doOrders game =
  let (p1, p2)  = findBestPair game
      selfLoc   = self^.playerLoc
      p1Loc     = p1^.passengerLoc
      p2Loc     = p2^.passengerLoc
      Just self = getPlayerByGuid (game^.players) (game^.myGuid)
      curTile   = (game^.gameMap.tiles) ! (selfLoc^.x, selfLoc^.y)
      tile1     = (game^.gameMap.tiles) ! (p1Loc^.x, p1Loc^.y)
      tile2     = (game^.gameMap.tiles) ! (p2Loc^.x, p2Loc^.y)
  in
  case pathToDestination (game^.gameMap) curTile tile1 of
    Nothing   -> []
    Just path -> [Move (map (^.tileLoc) path) [p1, p2]]


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

heuristic :: Game -> Location -> Passenger -> Passenger -> Int
heuristic game curLoc one two = 
  let mp = game^.gameMap
      oneRoute = head $ one^.route
      twoRoute = head $ two^.route
      curTile = tileAt mp curLoc
      dToFirst = pathToDestination mp curTile (tileAt mp (one^.passengerLoc))
      firstToDest = pathToDestination mp (tileAt mp (one^.passengerLoc)) (tileAt mp (oneRoute^.companyLoc))
      firstDestToSecond = pathToDestination mp  (tileAt mp (oneRoute^.companyLoc)) (tileAt mp (two^.passengerLoc))
      secondToDest = pathToDestination mp (tileAt mp (two^.passengerLoc)) (tileAt mp (two^.passengerLoc)) in
    length dToFirst + length firstToDest + length firstDestToSecond + length secondToDest

findBestPair :: Game -> (Passenger, Passenger)
findBestPair game = bestPair
  where
    bestPair = fst $ maximumBy (compare `on` snd) $ zip allPairs scores 
    allPairs = filter allowed $ map pairToTuple $ sequence [availablePassengers, availablePassengers]
    Just self = getPlayerByGuid (game^.players) (game^.myGuid)
    scores = map (uncurry $ heuristic game (self^.playerLoc)) allPairs
    allowed (x, y) = (x ^. passengerName) /= (y ^. passengerName)
    availablePassengers = filter isAvaliable (game ^. passengers)
    isAvaliable passenger = (passenger ^. passengerStatus) == Waiting
    pairToTuple [a, b] = (a, b)
