module Main (main) where

import Game.Map ( Map, createMap )
import Game.Renderer ( renderMap )
import Game.Engine ( update )
import Control.Concurrent ( threadDelay )


{- Config -}

fps :: Int
fps = 24

frameTime :: Int
frameTime = round ((1 / (fromIntegral fps :: Float)) * (10^6))


{- Main Loop (Run every frame) -}
-- gameLoop :: [[MapCell]] -> IO ()
-- gameLoop mp = do
--     rerender mp
--     threadDelay frameTime
--     gameLoop mp


main :: IO ()
main = update (createMap 10 10) 5 ""    -- using engine
--main = gameLoop (createMap 10 10)     -- using game loop (and fps)
