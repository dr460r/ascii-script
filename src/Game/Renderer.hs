module Game.Renderer
( renderMap, renderText, resetRender )
where

import System.Console.ANSI
import Game.Map ( MapTile, Map, Terrain (..), LandType (..), WaterType (..), Object (..))
import System.IO (stdout, hFlush)

{- Color controls -}

-- Sets color depending on cell data
setColor :: MapTile -> IO ()
setColor (Water Fresh, _, _, _) = setSGR [SetColor Background Vivid Cyan]
setColor (Land Arable, _, _, _) = setSGR [SetColor Background Dull Green]
setColor (_, Crop _, _, _) = setSGR [SetColor Background Dull Green]
setColor _ = resetColor -- catch all

resetColor :: IO ()
resetColor = setSGR [Reset]

{- Utils -}

resetRender :: IO ()
resetRender = do
    clearScreen
    setCursorPosition 0 0


{- Map rendering logic -}

-- Render Map
renderMap :: Map -> IO ()
renderMap mp = do
    renderMapLoop mp
    resetColor
    hFlush stdout


-- Recursive loop to render map
renderMapLoop :: Map -> IO ()

renderMapLoop [] = do                   -- when map is finished rendering
    putStrLn ""

renderMapLoop ([]:cls) = do             -- when one row of map is finished rendering
    putStrLn ""
    renderMap cls

renderMapLoop ((cl:clr):cls) = do       -- when actual tile rendering is happenging
    renderMapTile cl
    renderMap (clr:cls)


-- Render Map Cell
renderMapTile :: MapTile -> IO ()
renderMapTile cl = do
    setColor cl
    putStr "  "  -- TODO: Render actual character (this is temporal, just to check color)


{-  -}

-- Render UI
renderText :: String -> IO ()
renderText s = do
    resetColor
    putStr s
    hFlush stdout
