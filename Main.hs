{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import ClassyPrelude
import XML
import WindwardopolisClient
import Types
import Graph

-- main = do 
--   contents <- readFile "test.xml"
--   parsed <- readSetup contents
--   print "Parsed."

main :: IO ()
main = do
  args <- getArgs
  let Just serverAddr = headMay args <|> Just "127.0.0.1"
  
  putStrLn$ "Connecting to "++serverAddr
  
  runClient (unpack serverAddr) "1707" $ \(send, get) -> do
    send "<join name='hi' school='foobar' language='haskell'/>"
    msg <- get
    putStrLn$ pack msg


herusticFun :: Passenger -> Passenger -> Int
herusticFun passenger1 passenger2

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
    
    
    
    
    
    
    