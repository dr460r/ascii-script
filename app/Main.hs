module Main (main) where

import Game.Engine ( update )
import Game.Data.Map (createMap)

main :: IO ()
main = update (createMap 10 10, 50) ""
