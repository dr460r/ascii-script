module Main (main) where

import Game.Engine ( update )
import Game.Loader ( loadMap )
import System.IO ( hSetBuffering, stdin, BufferMode (..), hSetEcho )

main :: IO ()
main = do
    m <- loadMap "res/maps/default"
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    update (m, 200) ""

