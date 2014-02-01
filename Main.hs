{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import ClassyPrelude
import XML
import WindwardopolisClient
import Types
import Graph

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
    SetupMessage game <- get
    putMVar stateVar game

    forever $ do
      -- read an update
      UpdateMessage update <- get
      modifyMVar_ stateVar (updateState update)

      -- compute orders and send them
      orders <- doOrders <$> readMVar stateVar
      send orders

doOrders :: Game -> Command
doOrders = error "not implemented yet"

{-

herusticFun :: Passenger -> Passenger -> Int
herusticFun passenger1 passenger2 = 1

findBestPair :: Game -> Tuple Passenger Passenger
findBestPair game = 
  where 
    isAvaliable passenger = (passenger ^. Status) == Unserviced
    avaliablePassengers = filter isAvaliable (game ^. passengers)
    allPairs = sequence [game ^. passengers, game ^. passengers]
    maximumBy (compare `on` snd) $ map (\[passenger1, passenger2] -> 
      (
        (passenger1, passenger2),
        (herusticFun passenger1 passenger2)
      )
    ) allPairs

-}
