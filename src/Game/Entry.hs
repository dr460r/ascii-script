module Game.Entry (entryPoint) where

import System.IO ( hSetBuffering, stdin, BufferMode (..), hSetEcho )

import Game.Data.Map ( mapSize )
import Game.Config ( housePop )
import Game.Loader ( loadMap )
import Game.Engine ( update )

startRes :: Int
startRes = 75

entryPoint :: IO ()
entryPoint = do
    m <- loadMap "res/maps/default"
    let sz = mapSize m
    let sz' = (ceiling (itf (fst sz) / 2), ceiling (itf (snd sz) / 2))
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    update (m, startRes * 10, housePop, sz') ""

itf :: Int -> Float
itf = fromIntegral