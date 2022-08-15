module Game.Map
( MapCell
, initialMap
, createMap
)
where

type MapCell = Int

initialMap :: [[MapCell]]
initialMap = createMap 3 3

createMap :: Int -> Int -> [[MapCell]]
createMap w h = replicate h (replicate w 0)