module Game.Engine
( update
, GameState
)
where

import Data.List.Split ( splitOn )
import Game.Renderer ( render )
import Game.Data.Map


{- Main Loop -}
update :: GameState -> String -> IO ()
update st com = do
    let st' = processCmd com st
    render st'
    com' <- getLine
    update (passTime st') com'


{-== Game State Update Logic ==-}

{- Pass Time -}
passTime :: GameState -> GameState
passTime = updateMap -- TODO: Add logic for time passing (integer that represents days, for example)

{- Update Map -}
updateMap :: GameState -> GameState
updateMap st@(mp, _) = updateTile st (getTile mp p) p where p = (0,0)

{- Update Tiles Recursively -}
updateTile :: GameState -> MapTile -> MapPos -> GameState
-- Crop Tile
updateTile (mp, res) (tb, Crop c, tu, te) pos
    | pos' == (0,0) = st'
    | otherwise     = updateTile st' (getTile mp pos') pos'
    where st' = (changeTile mp pos (tb, Crop (c+1), tu, te), res)
          pos' = nextTilePos mp pos
-- catch-all
updateTile st@(mp, _) _ pos
    | pos' == (0,0) = st
    | otherwise     = updateTile st (getTile mp pos') pos'
    where pos' = nextTilePos mp pos

{- Get Position of next Tile -}
nextTilePos :: Map -> MapPos -> MapPos
nextTilePos mp (x,y) = (if x+1 < w then x+1 else 0, if y+1 < h then y+1 else 0)
    where msz = mapSize mp
          w = fst msz
          h = snd msz


{-== Command Processing Logic ==-}

{- Process cmd -}
processCmd :: String -> GameState -> GameState
processCmd cmd = procCmdMap (head cmd') (cmd' !! 1)
                where cmd' = splitOn " " cmd


{- Command Mappings -}
procCmdMap :: String -> String -> GameState -> GameState
-- Plant Crop
procCmdMap "plant" "" st = st   -- no params given
procCmdMap "plant" prm (mp, res) = (changeTile mp pos tile', res - 1)
                                where pos = strToPos prm
                                      (tbase, _, tunit, teff) = getTile mp pos
                                      tile' = (tbase, Crop 6, tunit, teff)
-- catch-all
procCmdMap _ _ st = st


{- Command Processing Utils -}

-- " abc  " -> "abc"
trim :: String -> String
trim = unwords . words

-- "3, 5" -> (3,5)
strToPos :: String -> MapPos
strToPos s = (rdtr (head ar) :: Int, rdtr (ar !! 1) :: Int)
            where ar = splitOn "," s; rdtr = read . trim
