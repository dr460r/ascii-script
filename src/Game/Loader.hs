module Game.Loader
( loadMap
)
where

import Game.Data.Map
import Data.List.Split

loadMap :: String -> IO Map
loadMap name = do
    s <- readFile name
    return $ map (map parseTileTerrain) (mapStrToList s)

mapStrToList :: String -> [[String]]
mapStrToList = map (splitOn " ") . splitOn "\n"

parseTileTerrain :: String -> MapTile
parseTileTerrain "." = (Land Arable, NoObject, NoUnit, NoEffect)
parseTileTerrain "#" = (Land NonArable, NoObject, NoUnit, NoEffect)
parseTileTerrain "o" = (Water Fresh, NoObject, NoUnit, NoEffect)
parseTileTerrain "w" = (Water Salty, NoObject, NoUnit, NoEffect)
parseTileTerrain _ = (Land NonArable, NoObject, NoUnit, NoEffect) -- catch-all