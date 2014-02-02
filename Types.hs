{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import Data.Array
import Data.Monoid

data Location = Loc {
    _x :: Int,
    _y :: Int
  } deriving (Show, Eq)

makeLenses ''Location


data RoadDirection = NORTH_SOUTH | EAST_WEST | INTERSECTION | NORTH_UTURN
                   | EAST_UTURN | SOUTH_UTURN | WEST_UTURN | T_NORTH
                   | T_EAST | T_SOUTH | T_WEST | CURVE_NE | CURVE_NW
                   | CURVE_SE | CURVE_SW
  deriving (Show, Eq)

data StopDirection = StopEast | StopWest | StopNorth | StopSouth deriving (Show, Eq)

data TileType = BusStop | CoffeeBuilding | CoffeeStop | CompanyTile | Park | Road RoadDirection [StopDirection]
              deriving (Show, Eq)
makeLenses ''TileType

data Tile = Tile {
    _tileLoc :: Location,
    _tileType :: TileType
  }
  deriving (Show, Eq)
makeLenses ''Tile

instance Ord Tile where
    compare one two =   compare (one ^. tileLoc . x) (two ^. tileLoc . x)
                    <>  compare (one ^. tileLoc . y) (two ^. tileLoc . y)


data Map = Map {
  _width :: Int,
  _height :: Int,
  _unitsTile :: Int,
  _tiles :: Array (Int, Int) Tile
  }
  deriving Show
makeLenses ''Map

data Company = Company {
  _companyName :: String,
  _companyLoc :: Location
  }
  deriving Show
makeLenses ''Company

data PassengerStatus = Transit | Waiting deriving Show

data Passenger = Passenger {
  _worth :: Int,
  _route :: [Company],
  _enemies :: [Passenger],
  _passengerName :: String,
  _passengerLoc :: Location,
  _passengerStatus :: PassengerStatus
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
      | RelocateAllPassengers
      deriving Show
makeLenses ''Powerup

data Player = Player {
      _uuid :: String,
      _playerLoc :: Location,
      _playerAngle :: Int,
      _score :: Maybe Float,
      _scoreTotal :: Maybe Float,
      _coffees :: Maybe Int,
      _maxCards :: Maybe Int
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
  _myGuid :: String,
  _gameMap :: Map,
  _players :: [Player],
  _stores :: [Store],
  _companies :: [Company],
  _passengers :: [Passenger],
  _powerups :: [Powerup]
  }
  deriving Show
makeLenses ''Game

data OrderType = DrawCard | PlayCard | DiscardCard

instance Show OrderType where
  show DrawCard = "DRAW"
  show PlayCard = "PLAY"
  show DiscardCard = "DISCARD"

-- commands to send
type Path = [Location]
type Pickups = [Passenger]
data Command
     = Join {
        language :: String,
        teamName :: String,
        school :: String      
      }
    | Ready Path Pickups
    | Move Path Pickups
    | CardOrder OrderType Powerup
    deriving Show

data GameUpdate = NoPathUpdate Game
                | UpdateUpdate Game
    deriving Show

data Message = SetupMessage Game
             | UpdateMessage GameUpdate
             | Exit
             deriving Show
