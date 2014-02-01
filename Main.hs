module Main where

import XML

main = do 
  contents <- readFile "test.xml"
  parsed <- readSetup contents
  print "Parsed."
