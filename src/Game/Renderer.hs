module Game.Renderer
( render )
where

import System.Console.ANSI
    ( clearScreen,
      setCursorPosition,
      setSGR,
      Color(Cyan, Yellow, Green, Black),
      ColorIntensity(Vivid, Dull),
      ConsoleLayer(Background, Foreground),
      SGR(Reset, SetColor) )
import Game.Data.Map ( MapTile, Map, Terrain (..), LandType (..), WaterType (..), Object (..), GameState)
import System.IO (stdout, hFlush)


{- Reset Color -}
resetColor :: IO ()
resetColor = setSGR [Reset]


{- Utils -}

clearTerminal :: IO ()
clearTerminal = do
    clearScreen
    setCursorPosition 0 0


{- Game Renderer -}

render :: GameState -> IO ()
render (mp, res) = do
    clearTerminal
    renderMap mp
    renderText $ "resources: " ++ show res ++ "\n\n> "


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


-- Render Map Tile
renderMapTile :: MapTile -> IO ()
renderMapTile tile = do
    setColor tile
    putStr $ strForTile tile


{- Other rendering logic -}

-- Render UI
renderText :: String -> IO ()
renderText s = do
    resetColor
    putStr s
    hFlush stdout


{- Color Mappings -}
setColor :: MapTile -> IO ()

-- Crop Field
setColor (Land Arable, Crop _, _, _) = do
    setSGR [SetColor Background Vivid Yellow]
    setSGR [SetColor Foreground Dull Black]

-- Arable Land
setColor (Land Arable, _, _, _) = setSGR [SetColor Background Dull Green]

-- Fresh Water
setColor (Water Fresh, _, _, _) = setSGR [SetColor Background Vivid Cyan]

-- catch-all
setColor _ = resetColor


{- Character Mappings -}

strForTile :: MapTile -> String

-- Crop
strForTile (_, Crop x, _, _)
        | x' == 1 = "  "
        | x' == 2 = " ."
        | x' == 3 = ".."
        | x' == 4 = ".:"
        | x' == 5 = "::,"
        | x' == 6 = ":l"
        | x' == 7 = "ll"
        | x' == 8 = "lf"
        | x' == 9 = "ff"
        | x' == 0 = "##"
        where x' = x `mod` 10

-- catch-all
strForTile _ = "  "