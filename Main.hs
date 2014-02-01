{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import ClassyPrelude
import XML
import WindwardopolisClient


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
