{-# LANGUAGE RankNTypes, OverloadedStrings, NoImplicitPrelude #-}
module XML (
  parseMessage,
  encodeCommand,
  getCompany, getPlayer, getPassenger
  ) where

import ClassyPrelude hiding (Element, Map)
import Prelude (read)
import Text.XML as XML
import Text.XML.Lens as Lens
import Control.Lens
import qualified Data.Map as Map
import Data.List.Utils (split)
import Data.Maybe (fromMaybe, fromJust)
import Data.Array

import Types


subnodes :: Name -> Traversal' Element Element
subnodes str = entire . el str

int :: Text -> Int
int = read . unpack

float :: Text -> Float
float = read . unpack

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
          stopMay = parseStop . unpack <$> tile ^. attribute "stop-sign" 
          stop = fromMaybe [] stopMay in
        Tile (Loc (int x) (int y)) (Road dir stop)
    parseStop "STOP_EAST" =  [StopEast]
    parseStop "STOP_WEST" =  [StopWest]
    parseStop "STOP_NORTH" =  [StopNorth]
    parseStop "STOP_SOUTH" =  [StopSouth]
    parseStop compound = 
      let noSpaces = filter (/= ' ') compound
          stops = split "," compound in
        concatMap parseStop stops

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
        "CURVE_SE"     ->             CURVE_SE
        other -> error $ "unknown direction " ++ unpack other

parseMap :: Element -> Map
parseMap setup = theMap
  where
    [mapEl] = setup ^.. subnodes "map"
    mapSize = (int $ mapEl ^. attr "width", int $ mapEl ^. attr "height") :: (Int, Int)
    unitsTile = int $ mapEl ^. attr "units-tile"
    tiles = map parseTile $ mapEl ^.. subnodes "tile"

    mapWidth = fst mapSize
    mapHeight = snd mapSize
    tileArray = array ((0, 0), (mapWidth -1, mapHeight -1)) arrayAssocs
    arrayAssocs = map toAssoc tiles
    toAssoc tile = ((tile ^. tileLoc . x, tile ^. tileLoc . y), tile)

    theMap = Map {
      _width = mapWidth,
      _height = mapHeight,
      _unitsTile = unitsTile,
      _tiles = tileArray
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
          "RELOCATE_ALL_PASSENGERS" -> RelocateAllPassengers
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

parsePlayers :: [Player] -> Element -> [Player]
parsePlayers olds setup = players
  where
    players = setup ^.. subnodes "player" . to fromEl
    fromEl player =
      let uuid = unpack $ player ^. attr "guid"
          oldPlayer = getPlayerByGuid olds uuid in
        case oldPlayer of
          Nothing ->
            Player {
              _playerName = unpack $ player ^. attr "name",
              _uuid = uuid,
              _playerLoc = Loc (int $ player ^. attr "limo-x") (int $ player ^. attr "limo-y"),
              _playerAngle = int $ player ^. attr "limo-angle",
              _score  = float <$> player ^. attribute "score",
              _scoreTotal  = float <$> player ^. attribute "total-score",
              _coffees  = int <$> player ^. attribute "coffee-servings",
              _maxCards  = int <$> player ^. attribute "cards-max"
              }
          Just play ->
            play {
            _playerLoc = Loc (int $ player ^. attr "limo-x") (int $ player ^. attr "limo-y"),
            _playerAngle = int $ player ^. attr "limo-angle",
            _score  = float <$> player ^. attribute "score",
            _scoreTotal  = float <$> player ^. attribute "total-score",
            _coffees  = int <$> player ^. attribute "coffee-servings",
            _maxCards  = int <$> player ^. attribute "cards-max"
          }

getCompany :: [Company] -> String -> Company
getCompany [] name = error $ "No such company: '" ++ name ++ "'"
getCompany (comp:rest) name =
  if comp ^. companyName == name
  then comp
  else getCompany rest name

getPlayer :: [Player] -> String -> Player
getPlayer [] name = error $ "No such player: '" ++ name ++ "'"
getPlayer (play:rest) name =
  if play ^. playerName == name
  then play
  else getPlayer rest name

getPassenger :: [Passenger] -> String -> Passenger
getPassenger [] _ = error "No such passenger"
getPassenger (pass:rest) name =
  if pass ^. passengerName == name
  then pass
  else getPassenger rest name


parsePassengers :: Bool -> [Company] -> [Player] -> Element -> [Passenger]
parsePassengers isUpdate companies players setup = passengers
  where
    passengers = setup ^.. subnodes "passenger" . to fromEl
    fromEl pass =
      Passenger {
        _worth = int $ pass ^. attr "points-delivered",
        _passengerName = unpack $ pass ^. attr "name",
        _passengerLoc = parsePassLoc pass,
        _route = parseRoute pass,
        _enemies = parseEnemies pass,
        _passengerStatus = Waiting
        }
    parsePassLoc pass =
      let lobby =unpack <$> pass ^. attribute "lobby" 
          driver = unpack <$> pass ^. attribute "limo-driver" in
        case (lobby, driver) of
          (Nothing, Just name) -> getPlayer players name ^. playerLoc
          (Just name, Nothing) -> getCompany companies name ^. companyLoc
          _ -> error "Where is this guy?"

    parseRoute pass =
      if not isUpdate
      then let names = pass ^.. subnodes "route" . text . to unpack in
                map (getCompany companies) names
      else let names = pass ^. attr "route" . to (split ";" . unpack) in
                map (getCompany companies) names


    parseEnemies pass =
      let names = pass ^.. subnodes "enemy" . text . to unpack in
        map (getPassenger passengers) names

parseSetup :: String -> Game
parseSetup string = 
  let doc = parseText_ def (pack string) in
    case doc ^.. root . subnodes "setup" of
      [] -> error "No setup tag."
      [setup] ->
        let myId = unpack $ setup ^. attr "my-guid"
            map = parseMap setup
            players = parsePlayers [] setup
            stores = parseStores setup
            companies = parseCompanies setup
            passengers = parsePassengers False companies players setup
            powerups = parsePowerups companies passengers setup in
        Game {
          _myGuid = myId,
          _gameMap = map,
          _players = players,
          _stores = stores,
          _companies = companies,
          _passengers = passengers,
          _powerups = powerups
        }

parseStatus :: String -> Game -> GameUpdate
parseStatus str game = 
  let doc = parseText_ def (pack str) in
    case doc ^.. root . subnodes "status" of
      [] -> error "No status tag."
      [status] ->
        let cause = unpack $ status ^. attr "status"
            guid = unpack $ status ^. attr "player-guid"
            play = parsePlayers (game ^. players) status
            pass = parsePassengers True (game ^. companies) play status
            newGame = game & passengers .~ pass
                           & players .~ play
          in case cause of
               "NO_PATH" -> NoPathUpdate newGame
               "UPDATE" -> UpdateUpdate newGame
               "PASSENGER_PICKED_UP" -> PassengerPickedUpUpdate newGame
               "PASSENGER_NO_ACTION" -> PassengerNoActionUpdate newGame
               "PASSENGER_DELIVERED_AND_PICKED_UP" ->PassengerDeliveredAndPickedUpUpdate newGame 
               "PASSENGER_DELIVERED" ->PassengerDeliveredUpdate newGame 
               "PASSENGER_REFUSED_ENEMY" ->PassengerRefusedEnemyUpdate newGame 
               "COFFEE_STORE_CAR_RESTOCKED" -> CoffeeStoreCarRestockedUpdate newGame
               "PASSENGER_ABANDONED" -> PassengerAbandoned newGame
               _ -> UpdateUpdate newGame

encodeCommand :: Command -> String
encodeCommand cmd = drop 1 $  dropWhile (/= '>') $ unpack $ renderText def doc
  where
    prologue = Prologue [] Nothing []
    doc = Document prologue root []
    root = mkCmd cmd

    mkEl :: Name -> [(Name, Text)] -> [Node] -> Element
    mkEl name attrs = Element name (Map.fromList attrs)

    mkCmd :: Command -> Element
    mkCmd (Join lang name school) =
      mkEl "join"
          [("language", pack lang), ("name", pack name), ("school", pack school)]
          []
    mkCmd (Ready locs passes) = mkReady "ready" locs passes
    mkCmd (Move locs passes) = mkReady "move" locs passes
    mkCmd (CardOrder order powerup) = 
      mkEl "order" [("action", pack $ show order)] [NodeElement $ powEl powerup]

    mkReady str locs passes =
        mkEl str [] [
          NodeElement pathEl,
          NodeElement pickupEl
          ]
        where
          pathEl = mkEl "path" [] [NodeContent path]
          pickupEl = mkEl "pick-up" [] [NodeContent pickup]
          path = pack $ concatMap showLoc locs
          pickup = pack $ concatMap showPass passes
          showLoc loc = show (loc ^. x) ++ "," ++ show (loc^.y) ++ ";"
          showPass pass = pass ^. passengerName ++ ";"

    powEl pow =
      mkEl "powerup" (attrs & (mapped . _2) %~ pack) []
      where 
        attrs = 
          case pow of
            MultDeliverAt comp -> [("card", "MULT_DELIVER_AT_COMPANY"),
                                  ("company", comp^.companyName)]
            MultDeliverPassenger pass -> [("card", "MULT_DELIVERING_PASSENGER"),
                                         ("passenger", pass^.passengerName)]
            AllOthersQuarterSpeed -> [("card", "ALL_OTHER_CARS_QUARTER_SPEED")]
            ChangeDestination -> [("card", "CHANGE_DESTINATION")]
            MovePassenger -> [("card", "MOVE_PASSENGER")]
            SelfQuarterSpeed -> [("card", "MULT_DELIVERY_QUARTER_SPEED")]
            RelocateAllCars -> [("card", "RELOCATE_ALL_CARS")]
            RelocateAllPassengers -> [("card", "RELOCATE_ALL_PASSENGERS")]
            StopCar -> [("card", "STOP_CAR")]


data MessageType = Setup | Update | PowerupStat

parseMessage :: Maybe Game -> String -> Message
parseMessage game string =
  case find (isMessageType string) [Setup, Update, PowerupStat] of
    Just Setup -> SetupMessage $ parseSetup string
    Just Update -> UpdateMessage $ parseStatus string $ fromJust game
    Just PowerupStat -> UpdateMessage (PowerupStatus $ fromJust game)
    Nothing -> error "Unknown message type."
  where
    parsed = parseText_ def (pack string)
    isMessageType str Setup =
      not . null $ parsed ^.. root . subnodes "setup"
    isMessageType str Update =
      not . null $ parsed ^.. root . subnodes "status"
    isMessageType str PowerupStat =
      not . null $ parsed ^.. root . subnodes "powerup-status"
