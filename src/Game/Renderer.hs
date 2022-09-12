module Game.Renderer
( render )
where

import System.Console.ANSI
    ( clearScreen,
      setCursorPosition,
      setSGR,
      Color(Cyan, Yellow, Green, Black, White, Blue),
      ColorIntensity(Vivid, Dull),
      ConsoleLayer(Background, Foreground),
      SGR(Reset, SetColor) )
import Text.Printf
import Game.Data.Map ( MapTile, Map, Terrain (..), LandType (..), WaterType (..), Object (..), GameState, Effect (NoEffect), MapPos)
import System.IO (stdout, hFlush)


{- Reset Color -}
resetColor :: IO ()
resetColor = setSGR [Reset]


{- Utils -}

clearTerminal :: IO ()
clearTerminal = do
    clearScreen
    setCursorPosition 0 0

itf :: Int -> Float
itf = fromIntegral

showf :: Int -> Int -> String
showf d = printf ("%." ++ show d ++ "f") . (/(10^d)) . itf

{- Game Renderer -}

render :: GameState -> IO ()
render (mp, res, act, cr) = do
    clearTerminal
    renderMap mp cr
    renderText $ "resources: " ++ showf 1 res
    renderText $ " | actions left: " ++ show act ++ "\n\n"
    renderText "c - Plant crops\n"
    renderText "h - Build house\n"
    renderText "(everything is placed on the tile where is cursor currently)\n"


{- Map rendering logic -}

-- Render Map
renderMap :: Map -> MapPos -> IO ()
renderMap mp cr = do
    renderMapLoop mp (0,0) cr
    resetColor
    hFlush stdout


-- Recursive loop to render map (map -> current pos -> cursor pos -> IO)
renderMapLoop :: Map -> MapPos -> MapPos -> IO ()

renderMapLoop [] _ _ = do                           -- when map is finished rendering
    putStr ""

renderMapLoop ([]:cls) (_,y) cr = do                -- when one row of map is finished rendering
    putStrLn ""
    renderMapLoop cls (0,y+1) cr

renderMapLoop ((cl:clr):cls) (x,y) (cx,cy) = do     -- when actual tile rendering is happenging
    renderMapTile cl (x == cx && y == cy)
    renderMapLoop (clr:cls) (x+1,y) (cx,cy)


-- Render Map Tile (map tile -> to render cursor -> IO)
renderMapTile :: MapTile -> Bool -> IO ()
renderMapTile tile cr = do
    --setColor tile
    let str = strForTile tile
    if cr then setSGR [SetColor Background Vivid Black] else setColor tile --EXP
    --putStr $ if cr then "+" ++ tail str else str                      --EXP
    putStr str


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
setColor (_, Crop _, _, NoEffect) = do
    setSGR [SetColor Background Vivid Yellow]
    setSGR [SetColor Foreground Dull Black]
-- House
setColor (_ , House _, _, NoEffect) = do
    setSGR [SetColor Foreground Dull Black]
-- Arable Land
setColor (Land Arable, _, _, NoEffect) = do
    setSGR [SetColor Background Dull Green]
    setSGR [SetColor Foreground Dull Black]
-- Non Arable Land
setColor (Land NonArable, _, _, NoEffect) = do
    setSGR [SetColor Background Dull Yellow]
    setSGR [SetColor Foreground Dull White]
-- Fresh Water
setColor (Water Fresh, _, _, NoEffect) = do
    setSGR [SetColor Background Vivid Cyan]
    setSGR [SetColor Foreground Dull Cyan]
-- Fresh Water
setColor (Water Salty, _, _, NoEffect) = do
    setSGR [SetColor Background Vivid Blue]
    setSGR [SetColor Foreground Dull Blue]
-- catch-all
setColor _ = resetColor


{- Character Mappings -}
strForTile :: MapTile -> String

-- Crop
strForTile (_, Crop x, _, _)
        | x == 0    = ";;"
        | x < 5     = "ii"
        | otherwise = "ll"
-- House
strForTile (_, House _, _, _) = "┌┐"
-- catch-all
strForTile _ = "  "