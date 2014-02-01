{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens

data Location = Loc {
    _x :: Int,
    _y :: Int
  } deriving Show

makeLenses ''Location

data TileType = BusStop | CoffeeBuilding | CoffeeStop | CompanyTile | Park | Road 
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
  _enemies :: [String],
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
    _storeLocation :: [Location]
  }
  deriving Show
makeLenses ''Store

