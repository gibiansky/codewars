{-# LANGUAGE OverloadedStrings #-}
module XML (
  test              
  ) where

import Text.XML as XML

test :: IO ()
test = do
  doc <- XML.readFile def "test.xml"
  print doc
