{-# LANGUAGE RankNTypes, OverloadedStrings, NoImplicitPrelude #-}
module XML (
  test              
  ) where

import ClassyPrelude hiding (Element)
import Prelude (read)
import Text.XML as XML
import Text.XML.Lens as Lens
import Control.Lens

import Types


subnodes :: Name -> Traversal' Element Element
subnodes str = entire . el str

int :: Text -> Int
int = read . unpack

parseTileType :: Text -> TileType
parseTileType "PARK" = Park
parseTileType "BUS_STOP" = BusStop
parseTileType "COFFEE_BUILDING" = CoffeeBuilding
parseTileType "ROAD" = Road
parseTileType "COMPANY" = CompanyTile
parseTileType "COFFEE_STOP" = CoffeeStop

parseTile tile = 
  let x = tile ^. attr "x"
      y = tile ^. attr "y"
      ty = tile ^. attr "type" in
    Tile (Loc (int x) (int y)) (parseTileType ty)

test :: IO ()
test = do
  doc <- XML.readFile def "test.xml"
  case doc ^.. root . subnodes "setup" of
    [] -> error "No setup tag."
    [setup] -> do
      let [map] = setup ^.. subnodes "map"
          mapSize = (int $ map ^. attr "width", int $ map ^. attr "height") :: (Int, Int)
          tiles = map ^.. el "tile"
      print $ setup^.. entire
                  . el "powerups"
