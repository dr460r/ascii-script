module Main (main) where

import Game.Map ( MapCell, createMap )
import Game.Renderer ( renderMap )
import Control.Concurrent ( threadDelay )



fps :: Int
frameTime :: Int
gameLoop :: [[MapCell]] -> MapCell -> IO b

{- Config -}

fps = 24


{- Setup -}

frameTime = round ((1 / (fromIntegral fps :: Float)) * (10^6))


{- Main Loop -}

gameLoop mp x = do
    let newMap = (x : tail (head mp)) : tail mp

    renderMap newMap
    threadDelay frameTime
    gameLoop newMap (x+1)


main :: IO ()
main = gameLoop (createMap 10 10) 0
