module Graph where

import Data.Array

import Types
import Control.Lens

neighbors :: Map -> Tile -> [Tile]
neighbors gameMap tile = neighborTiles
  where
    (ix, iy) = (tile ^. tileLoc . x, tile ^. tileLoc . y)
    indices = filter isValidIndex 
      [ (ix + 1, iy) ,
        (ix - 1, iy) ,
        (ix, iy + 1) ,
        (ix, iy - 1) ]

    isValidIndex (ix, iy) = ix >= 0 && iy >= 0 &&
                            ix < mapWidth && iy < mapHeight
    neighborTiles = filter isValidTile $ map (tileArray !) indices
    isValidTile tile = 
      case tile ^. tileType of
        Road dir stop -> True
        BusStop -> True
        CoffeeStop -> True
        _ -> False
    mapWidth = gameMap ^. width
    mapHeight = gameMap ^. height
    tileArray = array ((0, 0), (mapWidth -1, mapHeight -1)) arrayAssocs
    arrayAssocs = map toAssoc $ gameMap ^. tiles
    toAssoc tile = ((tile ^. tileLoc . x, tile ^. tileLoc . y), tile)
