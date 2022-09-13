module Game.Data.Map
( MapTile
, Map
, MapPos
, Terrain (..)
, LandType (..)
, WaterType (..)
, Object (..)
, Effect (..)
, Unit (..)
, GameState
, initialMap
, createMap
, changeTile
, getTile
, mapSize
, hasTerrain
, setEffectOnPos
)
where

{- Data Types -}

data Terrain = Land LandType | Water WaterType
data LandType = Arable | NonArable
data WaterType = Salty | Fresh

data Object = Crop Int | House Int | NoObject
data Effect = Fire Int Int | NoEffect           -- Fire Level SpreadPower
data Unit = Rats | NoUnit

type MapTile = (Terrain, Object, Unit, Effect)
type Map = [[MapTile]]

type MapPos = (Int, Int)

type GameState = (Map, Int, Int, MapPos) -- map, resources, actions, cursor

-- Instances of Eq

instance Eq Terrain where
    Land t1 == Land t2 = t1 == t2
    Water t1 == Water t2 = t1 == t2
    _ == _ = False

instance Eq LandType where
    Arable == Arable = True
    NonArable == NonArable = True
    _ == _ = False

instance Eq WaterType where
    Fresh == Fresh = True
    Salty == Salty = True
    _ == _ = False

{- Utils -}

changeTile :: Map -> MapPos -> MapTile -> Map
changeTile mp (x,y) tl = take y mp ++ [take x trg ++ [tl] ++ drop (x+1) trg] ++ drop (y+1) mp
                    where trg = mp !! y

getTile :: Map -> MapPos -> MapTile
getTile mp (x,y) = mp !! y !! x

mapSize :: Map -> (Int, Int)
mapSize [] = (0,0)
mapSize ([]:_) = (0,0)
mapSize mp = (length $ head mp, length mp)

isPosValid :: Map -> MapPos -> Bool
isPosValid mp (x,y) = x >= 0 && y >= 0 && x < w && y < h
    where
        sz = mapSize mp
        w = fst sz
        h = snd sz

hasTerrain :: Terrain -> MapTile -> Bool
hasTerrain trn (trn', _, _, _) = trn == trn'

setEffectOnPos :: Map -> Effect -> MapPos -> Map
setEffectOnPos mp ef pos = if isPosValid mp pos && valid then changeTile mp pos tile' else mp
    where
        tile@(tb, to, tu, _) = getTile mp pos
        tile' = (tb, to, tu, ef)
        valid = canSetEffectOnTile tile

canSetEffectOnTile :: MapTile -> Bool
canSetEffectOnTile (Land _, _, _, NoEffect) = True
canSetEffectOnTile (_, _, _, _) = False

{- Map creation -}

initialMap :: [[MapTile]]
initialMap = createMap 10 10

createMap :: Int -> Int -> [[MapTile]]
createMap w h = replicate h (replicate w (Land Arable, NoObject, NoUnit, NoEffect))

