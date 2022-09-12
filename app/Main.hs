module Main (main) where

import Game.Engine ( update, housePop )
import Game.Loader ( loadMap )
import System.IO ( hSetBuffering, stdin, BufferMode (..), hSetEcho )
import Game.Data.Map ( mapSize )

startRes :: Int
startRes = 30

main :: IO ()
main = do
    m <- loadMap "res/maps/default"
    let sz = mapSize m
    let sz' = (ceiling (itf (fst sz) / 2), ceiling (itf (snd sz) / 2))
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    update (m, startRes * 10, housePop, sz') ""

itf :: Int -> Float
itf = fromIntegral