module Game.Engine
( update
)
where

import Data.List.Split
import Game.Renderer ( renderMap, renderText, resetRender )
import Game.Map
    ( Unit(NoUnit),
      Object(NoObject),
      WaterType(Fresh),
      Terrain(Water),
      Effect(NoEffect),
      Map,
      changeTile )

{- Main Loop -}

-- map -> resources -> command
update :: Map -> Int -> String -> IO ()
update mp res com = do

    resetRender         -- clearing terminal
    renderMap mp        -- printing map

    renderText $ "resources: " ++ show res ++ "\n\n> "

    let nmp = processCmd com mp -- new map
    
    inp <- getLine      -- reading command from user

    update nmp (if res > 0 then res - 1 else 0) inp


{- Logic -}

-- Process user command
processCmd :: String -> Map -> Map
processCmd cmd = procCmdMap  (head $ splitOn " " cmd) ""

-- Process cmd mappings (cmd -> params -> map -> newmap)
procCmdMap :: String -> String -> Map -> Map
procCmdMap _ _ mp = changeTile mp (3,3) (Water Fresh, NoObject, NoUnit, NoEffect)

