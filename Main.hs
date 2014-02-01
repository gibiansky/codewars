{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import ClassyPrelude
import XML
import WindwardopolisClient
import Types

main :: IO ()
main = do
  args <- getArgs
  let Just serverAddr = headMay args <|> Just "127.0.0.1"
  
  putStrLn$ "Connecting to "++serverAddr
  
  runClient (unpack serverAddr) "1707" $ \(send, get) -> do
    -- Join the game
    send $ Join "Brainfuck" "ÜNICÖDE Y Ü NO CÓDE?!" "Harvard Med" 

    stateVar <- newEmptyMVar
    forever $ do
      -- intialize game
      SetupMessage game <- get
      putMVar stateVar game

      -- send initial orders
      let orders = doOrders game 
      send orders

doOrders :: Game -> Command
doOrders = undefined
