module Main where

import XML

main = do 
  parsed <- readSetup "test.xml"
  print "Parsed."
