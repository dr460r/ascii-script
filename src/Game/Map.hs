module Game.Map
( MapTile
, Map
, Terrain (..)
, LandType (..)
, WaterType (..)
, Object (..)
, Effect (..)
, Unit (..)
, initialMap
, createMap
, changeTile
)
where

{- Data Types -}

data Terrain = Land LandType | Water WaterType
data LandType = Arable | NonArable
data WaterType = Salty | Fresh

data Object = Crop Int | NoObject
data Effect = Fire Int | NoEffect
data Unit = Rats | NoUnit

type MapTile = (Terrain, Object, Unit, Effect)
type Map = [[MapTile]]


{- Utils -}

changeTile :: Map -> (Int, Int) -> MapTile -> Map
changeTile mp (x,y) tl = take y mp ++ [take x trg ++ [tl] ++ drop (x+1) trg] ++ drop (y+1) mp
                    where trg = mp !! y


{- Map creation -}

initialMap :: [[MapTile]]
initialMap = createMap 10 10

createMap :: Int -> Int -> [[MapTile]]
createMap w h = replicate h (replicate w (Land Arable, NoObject, NoUnit, NoEffect))

