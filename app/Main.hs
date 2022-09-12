module Main (main) where

import Game.Engine ( update )
import Game.Data.Map (createMap)
import Game.Loader ( loadMap )

main :: IO ()
main = do
    m <- loadMap "res/maps/default"
    update (m, 200) ""

