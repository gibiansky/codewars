{-# LANGUAGE RankNTypes, OverloadedStrings, NoImplicitPrelude #-}
module XML (
  readSetup              
  ) where

import ClassyPrelude hiding (Element, Map)
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
parseTileType "COMPANY" = CompanyTile
parseTileType "COFFEE_STOP" = CoffeeStop

parseTile tile = 
  let x = tile ^. attr "x"
      y = tile ^. attr "y"
      ty = tile ^. attr "type" in
    case ty of
      "ROAD" -> parseRoad x y tile
      ty -> Tile (Loc (int x) (int y)) (parseTileType ty)
  where
    parseRoad x y tile =
      let dir = parseDir $ tile ^. attr "direction"
          stop = parseStop <$> tile ^. attribute "stop-sign"  in
        Tile (Loc (int x) (int y)) (Road dir stop)
    parseStop "STOP_EAST" =  StopEast
    parseStop "STOP_WEST" =  StopWest
    parseStop "STOP_NORTH" =  StopNorth
    parseStop "STOP_SOUTH" =  StopSouth

    parseDir dir = 
      case dir of
        "NORTH_SOUTH"  ->              NORTH_SOUTH
        "EAST_WEST"    ->              EAST_WEST
        "INTERSECTION" ->              INTERSECTION
        "NORTH_UTURN"  ->              NORTH_UTURN
        "EAST_UTURN"   ->              EAST_UTURN
        "SOUTH_UTURN"  ->              SOUTH_UTURN
        "WEST_UTURN"   ->              WEST_UTURN
        "T_NORTH"      ->              T_NORTH
        "T_EAST"       ->              T_EAST
        "T_SOUTH"      ->              T_SOUTH
        "T_WEST"       ->              T_WEST
        "CURVE_NE"     ->              CURVE_NE
        "CURVE_NW"     ->              CURVE_NW
        "CURVE_SW"     ->              CURVE_SW

parseMap :: Element -> Map
parseMap setup = theMap
  where
    [mapEl] = setup ^.. subnodes "map"
    mapSize = (int $ mapEl ^. attr "width", int $ mapEl ^. attr "height") :: (Int, Int)
    unitsTile = int $ mapEl ^. attr "units-tile"
    tiles = map parseTile $ mapEl ^.. subnodes "tile"
    theMap = Map {
      _width = fst mapSize,
      _height = snd mapSize,
      _unitsTile = unitsTile,
      _tiles = tiles
      }

parsePowerups :: [Company] -> [Passenger] -> Element -> [Powerup]
parsePowerups companies passengers setup = powerups
  where
    powerups = setup ^.. subnodes "powerup" . to fromEl
    fromEl pow =
      let powType = pow ^. attr "card" in
        case powType of
          "MULT_DELIVER_AT_COMPANY" ->
            MultDeliverAt $ getCompany companies $ unpack $ pow ^. attr "company"
          "MULT_DELIVERING_PASSENGER" ->
            MultDeliverPassenger $ getPassenger passengers $ unpack $ pow ^. attr "passenger"
          "ALL_OTHER_CARS_QUARTER_SPEED" -> AllOthersQuarterSpeed
          "CHANGE_DESTINATION" -> ChangeDestination
          "MOVE_PASSENGER" -> MovePassenger
          "MULT_DELIVERY_QUARTER_SPEED" -> SelfQuarterSpeed
          "RELOCATE_ALL_CARS" -> RelocateAllCars
          "RELOCATE_ALL_PASSENGERS" -> ReloacteAllPassengers
          "STOP_CAR" -> StopCar
          

parseStores :: Element -> [Store]
parseStores setup = stores
  where
    stores = setup ^.. subnodes "store" . to fromEl
    fromEl player =
      Store {
        _storeName = unpack $ player ^. attr "name",
        _storeLocation = Loc (int $ player ^. attr "bus-stop-x") (int $ player ^. attr "bus-stop-y")
        }

parseCompanies :: Element -> [Company]
parseCompanies setup = companies
  where
    companies = setup ^.. subnodes "company" . to fromEl
    fromEl company =
      Company {
        _companyName = unpack $ company ^. attr "name",
        _companyLoc = Loc (int $ company ^. attr "bus-stop-x") (int $ company ^. attr "bus-stop-y")
        }

parsePlayers :: Element -> [Player]
parsePlayers setup = players
  where
    players = setup ^.. subnodes "player" . to fromEl
    fromEl player =
      Player {
        _uuid = unpack $ player ^. attr "guid",
        _playerLoc = Loc (int $ player ^. attr "limo-x") (int $ player ^. attr "limo-y"),
        _playerAngle = int $ player ^. attr "limo-angle"
        }

getCompany :: [Company] -> String -> Company
getCompany [] _ = error "No such company"
getCompany (comp:rest) name =
  if comp ^. companyName == name
  then comp
  else getCompany rest name

getPassenger :: [Passenger] -> String -> Passenger
getPassenger [] _ = error "No such company"
getPassenger (pass:rest) name =
  if pass ^. passengerName == name
  then pass
  else getPassenger rest name


parsePassengers :: [Company] -> Element -> [Passenger]
parsePassengers companies setup = passengers
  where
    passengers = setup ^.. subnodes "passenger" . to fromEl
    fromEl pass =
      Passenger {
        _worth = int $ pass ^. attr "points-delivered",
        _passengerName = unpack $ pass ^. attr "name",

        _route = parseRoute pass,
        _enemies = parseEnemies pass
        }
    parseRoute pass =
      let names = pass ^.. subnodes "route" . text . to unpack in
        map (getCompany companies) names

    parseEnemies pass =
      let names = pass ^.. subnodes "enemy" . text . to unpack in
        map (getPassenger passengers) names

readSetup :: String -> IO Game
readSetup string = 
  let doc = parseText_ def (pack string) in
    case doc ^.. root . subnodes "setup" of
      [] -> error "No setup tag."
      [setup] -> do
        let map = parseMap setup
            players = parsePlayers setup
            stores = parseStores setup
            companies = parseCompanies setup
            passengers = parsePassengers companies setup
            powerups = parsePowerups companies passengers setup
        return Game {
          _gameMap = map,
          _players = players,
          _companies = companies,
          _passengers = passengers,
          _powerups = powerups
        }
