module Game.Engine
( update
, housePop
)
where

import Data.List.Split ( splitOn )
import Game.Renderer ( render )
import Game.Data.Map
import System.IO ( stdin, hReady )

{- Conf -}
cropPrice :: Int
cropActons :: Int
baseCropYield :: Int

cropPrice = 50
cropActons = 1
baseCropYield = 1

housePop :: Int
housePrice :: Int
houseActions :: Int

housePop = 5
housePrice = 500
houseActions = 1


{- Main Loop -}
update :: GameState -> String -> IO ()
update st com = do
    let st' = processCmd com st
    render st'
    com' <- getKey
    update st' com'


{-== Game State Update Logic ==-}

{- Pass Time -}
passTime :: GameState -> GameState
passTime = updateMap -- TODO: Add logic for time passing (integer that represents days, for example)

{- Update Map -}
updateMap :: GameState -> GameState
updateMap st@(mp, _, _, _) = updateTile st (getTile mp (0,0)) (0,0)

{- Update Tiles Recursively -}
updateTile :: GameState -> MapTile -> MapPos -> GameState

-- Crop Tile
updateTile (mp, res, act, cr) (tb, Crop _, tu, te) pos
    | pos' == (0,0) = st'
    | otherwise     = updateTile st' (getTile mp pos') pos'
    where
        c = cropFertility mp pos
        res' = res + baseCropYield + (if c > 0 then (if c > 4 then 2 else 1) else 0)
        st'  = (changeTile mp pos (tb, Crop c, tu, te), res', act, cr)
        pos' = nextTilePos mp pos

-- House Tile
updateTile (mp, rs, ac, cr) (_, House x, _, _) _ = (mp, rs, ac + x, cr)

-- catch-all
updateTile st@(mp, _, _, _) _ pos
    | pos' == (0,0) = st
    | otherwise     = updateTile st (getTile mp pos') pos'
    where
        pos' = nextTilePos mp pos

{- Get Position of next Tile -}
nextTilePos :: Map -> MapPos -> MapPos
nextTilePos mp (x,y) = (x', y')
    where msz = mapSize mp
          w = fst msz
          h = snd msz
          x' = if x+1 < w then x+1 else 0
          y' = if x' == 0 then (if y+1 < h then y+1 else 0) else y

{- Count Surrounding Tiles -}
-- fn is function that maps MapTile data to Bool (for example: hasTerrain Land Arable, that returns True if the thile is Arable)
countSurrTiles :: Map -> MapPos -> (MapTile -> Bool) -> Int
countSurrTiles mp (x,y) fn = cU + cD + cL + cR + cUL + cUR + cDL + cDR
    where
        -- checks:
        msz = mapSize mp; w = fst msz; h = snd msz
        u = x > 0; d = x < h - 1; l = x > 0; r = x < w - 1
        -- counts:
        cU = bti (u && fn (getTile mp (x, y-1))) -- Up
        cD = bti (d && fn (getTile mp (x, y+1))) -- Down
        cL = bti (l && fn (getTile mp (x-1, y))) -- Left
        cR = bti (r && fn (getTile mp (x+1, y))) -- Right
        cUL = bti (u && l && fn (getTile mp (x-1,y-1))) -- Up-Left
        cUR = bti (u && r && fn (getTile mp (x+1,y-1))) -- Up-Right
        cDL = bti (d && l && fn (getTile mp (x-1,y+1))) -- Down-Left
        cDR = bti (d && r && fn (getTile mp (x+1,y+1))) -- Down-Right


{-== Command Processing Logic ==-}

{- Combines multy-char keys into one String -}
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)


{- Process cmd -}
processCmd :: String -> GameState -> GameState
-- Next Turn (Pass time)
processCmd " " (m, r, _, c) = passTime (m, r, 0, c)
processCmd "\n" st = processCmd " " st -- alternative (Linux only)
-- Cursor movement
processCmd "i" st = procCmdMap "cursor" "up" st
processCmd "k" st = procCmdMap "cursor" "down" st
processCmd "l" st = procCmdMap "cursor" "right" st
processCmd "j" st = procCmdMap "cursor" "left" st
-- alternatives (Linux only)
processCmd "\ESC[A" st = processCmd "i" st
processCmd "\ESC[B" st = processCmd "k" st
processCmd "\ESC[C" st = processCmd "l" st
processCmd "\ESC[D" st = processCmd "j" st
-- Other Actions
processCmd "x" st@(_, _, _, cr) = procCmdMap "house" (showTuple cr) st
processCmd "c" st@(_, _, _, cr) = procCmdMap "crop" (showTuple cr) st
processCmd "v" st@(_, _, _, cr) = procCmdMap "fight" (showTuple cr) st
-- alternatives
processCmd "h" st = processCmd "x" st
processCmd "u" st = processCmd "c" st
processCmd "y" st = processCmd "v" st
processCmd _ st = procCmdMap "" "" st


{- Command Mappings -}
procCmdMap :: String -> String -> GameState -> GameState

-- Spawn Crop
procCmdMap "crop" prm st@(mp, _, _, _) = spawnObject st pos (Crop (cropFertility mp pos)) cropPrice cropActons
    where pos = strToPos prm

-- Spawn House
procCmdMap "house" prm st = spawnObject st pos (House housePop) housePrice houseActions
    where pos = strToPos prm

-- Cursor movements
procCmdMap "cursor" "up"    (mp, rs, ac, (x,y)) = (mp, rs, ac, (x,y')) where y' = if y-1 >= 0 then y-1 else y
procCmdMap "cursor" "down"  (mp, rs, ac, (x,y)) = (mp, rs, ac, (x,y')) where y' = if y+1 < snd (mapSize mp) then y+1 else y
procCmdMap "cursor" "right" (mp, rs, ac, (x,y)) = (mp, rs, ac, (x',y)) where x' = if x+1 < fst (mapSize mp) then x+1 else x
procCmdMap "cursor" "left"  (mp, rs, ac, (x,y)) = (mp, rs, ac, (x',y)) where x' = if x-1 >= 0 then x-1 else x

-- (DEV MODE)
-- Spawn Water
procCmdMap "/w" prm (mp, res, act, cr) = (changeTile mp pos tile', res, act, cr)
    where
        pos = strToPos prm
        (_, tobj, tunit, teff) = getTile mp pos
        tile' = (Water Fresh, tobj, tunit, teff)
-- catch-all
procCmdMap _ _ st = st


{-== Command Processing Utils ==-}
spawnObject :: GameState -> MapPos -> Object -> Int -> Int -> GameState
spawnObject st@(mp, res, act, cr) pos obj cost acost = if valid then (changeTile mp pos tile', res - cost, act - acost, cr) else st
    where
        tile@(tb, _, tu, te) = getTile mp pos
        tile' = (tb, obj, tu, te)
        valid = tileValidity tile' && res - cost >= 0 && act - acost >= 0 && canBuildOnTile tile

tileValidity :: MapTile -> Bool
tileValidity (Land _, NoObject, _, _) = True
tileValidity (Water _, NoObject, _, _) = True
tileValidity (Land Arable, Crop _, _, _) = True
tileValidity (Land _, House _, _, _) = True
tileValidity _ = False

canBuildOnTile :: MapTile -> Bool
canBuildOnTile (_, NoObject, _, _) = True
canBuildOnTile (_, _, _, _) = False

-- " abc  " -> "abc"
trim :: String -> String
trim = unwords . words

-- "3, 5" -> (3,5)
strToPos :: String -> MapPos
strToPos s = (rdtr (head ar) :: Int, rdtr (ar !! 1) :: Int)
            where ar = splitOn "," s; rdtr = read . trim


{-== Crop Utils ==-}
cropFertility :: Map -> MapPos -> Int
cropFertility mp pos = countSurrTiles mp pos (hasTerrain (Water Fresh))


{-== Utils ==-}

bti :: Bool -> Int
bti True = 1
bti False = 0

showTuple :: Show a => (a,a) -> String
showTuple (x,y) = show x ++ "," ++ show y