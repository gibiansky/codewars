{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import ClassyPrelude
import Data.Maybe
import XML
import WindwardopolisClient
import Types
import System.Exit
import Graph
import Data.List (maximumBy)

import Control.Lens

main :: IO ()
main = do
  args <- getArgs
  let Just serverAddr = headMay args <|> Just "127.0.0.1"
  
  putStrLn$ "Connecting to "++serverAddr
  
  runClient (unpack serverAddr) "1707" $ \(send, get) -> do
    -- Join the game
    send $ Join "Brainfuck" "ÜNICÖDE Y Ü NO CÓDE?!" "Harvard Med" 

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
  let (p1, p2) = findBestPair game in
  [
    Ready [p1^.passengerLoc, p2^.passengerLoc] [p1, p2]
  ]


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

heuristic :: Passenger -> Passenger -> Int
heuristic passenger1 passenger2 = 1

findBestPair :: Game -> (Passenger, Passenger)
findBestPair game = bestPair
  where
    isAvaliable passenger = (passenger ^. passengerStatus) == Waiting
    availablePassengers = filter isAvaliable (game ^. passengers)
    allPairs = filter allowed $ map pairToTuple $ sequence [availablePassengers, availablePassengers]
    allowed (x, y) = (x ^. passengerName) /= (y ^. passengerName)
    pairToTuple [a, b] = (a, b)
    scores = map (uncurry heuristic) allPairs
    bestPair = fst $ maximumBy (compare `on` snd) $ zip allPairs scores 
