module Graph where

import Data.Array
import Data.Graph.AStar
import qualified Data.Set as Set

import Types
import Control.Lens

neighbors :: Map -> Tile -> Set.Set Tile
neighbors gameMap tile = Set.fromList neighborTiles
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
    tileArray = gameMap ^. tiles
    mapWidth = gameMap ^. width
    mapHeight = gameMap ^. height
    isValidTile tile = 
      case tile ^. tileType of
        Road dir stop -> True
        BusStop -> True
        CoffeeStop -> True
        _ -> False

pathToDestination :: Map -> Tile -> Tile -> Maybe [Tile]
pathToDestination gameMap startTile endTile = aStar (neighbors gameMap) distanceFun heuristicFun goalFun startTile
  where
    distanceFun _ _ = 1
    heuristicFun tile = dx + dy
      where dx = tile ^. tileLoc . x - endTile ^. tileLoc . x 
            dy = tile ^. tileLoc . y - endTile ^. tileLoc . y
    goalFun = (== endTile)
