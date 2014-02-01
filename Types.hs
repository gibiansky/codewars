{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens

data Location = Loc {
    _x :: Int,
    _y :: Int
  } deriving Show

makeLenses ''Location


data RoadDirection = NORTH_SOUTH | EAST_WEST | INTERSECTION | NORTH_UTURN | EAST_UTURN | SOUTH_UTURN | WEST_UTURN | T_NORTH | T_EAST | T_SOUTH | T_WEST | CURVE_NE | CURVE_NW | CURVE_SE | CURVE_SW
  deriving Show
makeLenses ''RoadDirection

data Road = Road {
  direction :: RoadDirection
}
makeLenses ''Road

data TileType = BusStop | CoffeeBuilding | CoffeeStop | CompanyTile | Park | Road RoadDirection
              deriving Show
makeLenses ''TileType

data Tile = Tile {
    _tileLoc :: Location,
    _tileType :: TileType
  }
  deriving Show
makeLenses ''Tile

data Map = Map {
  _width :: Int,
  _height :: Int,
  _unitsTile :: Int,
  _tiles :: [Tile]
  }
  deriving Show
makeLenses ''Map

data Company = Company {
  _companyName :: String,
  _companyLoc :: Location
  }
  deriving Show
makeLenses ''Company

data Passenger = Passenger {
  _worth :: Int,
  _route :: [Company],
  _enemies :: [Passenger],
  _passengerName :: String
  }
  deriving Show
makeLenses ''Passenger

data Powerup
      = MultDeliverAt {
          _company :: Company
        }
      | MultDeliverPassenger {
          _passenger :: Passenger                      
        }
      | MovePassenger
      | ChangeDestination
      | AllOthersQuarterSpeed
      | SelfQuarterSpeed
      | StopCar
      | RelocateAllCars
      | ReloacteAllPassengers
      deriving Show
makeLenses ''Powerup

data Player = Player {
      _uuid :: String,
      _playerLoc :: Location,
      _playerAngle :: Int
    }
    deriving Show
makeLenses ''Player

data Store = Store {
    _storeLocation :: Location,
    _storeName :: String
  }
  deriving Show
makeLenses ''Store

data Game = Game {
  _gameMap :: Map,
  _players :: [Player],
  _companies :: [Company],
  _passengers :: [Passenger],
  _powerups :: [Powerup]
  }
