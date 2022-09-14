module Game.Renderer
( render
, renderText
)
where

import System.Console.ANSI
import System.IO (stdout, hFlush)
import Text.Printf

import Game.Data.Map ( MapTile, Map, Terrain (..), LandType (..), WaterType (..), Object (..), GameState, Effect (..), MapPos, mapSize)
import Game.Config ( fireMxLvl )


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
    renderText "\n"

    let w = fst (mapSize mp)
    renderResAct res act
    renderText "\n"
    renderText $ "Press [Space] key for NEXT TURN" ++ "\n\n\n"
    
    let infotitle = " INFO "
    renderText $ hline "*" (w - 3) ++ infotitle ++ hline "*" (w - 3) ++ "\n\n"
    
    renderText $ "> RESOURCES ($) are used for performing ACTIONS" ++ "\n"
    renderText $ "  - Each CROP field adds $ to total $ amount every turn" ++ "\n"
    renderText $ "  - Crop fields $ yield: [;;] 0.1, [ii] 0.2, [ll] 0.3" ++ "\n"
    renderText "\n"
    renderText $ "> MAN-POWER (#) is used for performing ACTIONS" ++ "\n"
    renderText $ "  - Every turn # resets to value that is proportional to number of HOUSES built" ++ "\n"
    renderText $ "  - Each house provides 5 #" ++ "\n"
    renderText "\n"

    renderText $ " Available ACTIONS" ++ "\n"
    renderKeyMappings
    renderText "\n"

    --renderText "(everything is placed on the tile where is cursor currently)\n"


renderResAct :: Int -> Int -> IO () -- └ ┘ ┼ ─ ┴ ├ ┤ ┬ ┌ ┐ │
renderResAct res act = do    
    let restbl = ["Resources", "$", showf 1 res]
    let acttbl = ["Man-power", "#", show act]
    renderText $ mkTbl [restbl] (map length restbl)
    renderText $ mkTbl [acttbl] (map length acttbl)

renderKeyMappings :: IO ()
renderKeyMappings = do
    let (cols, lens) = unzip [stdLen ["Key", "H","U","Y"]
            , stdLen ["Action", "Build House","Plant Crops","Fight Fire"]
            , stdLen ["Cost in $", "50.0","5.0","5.0"]
            , stdLen ["Cost in #", "1","1","1"]
            ]
    let rows = transpose cols

    let txt = mkTbl rows lens
    renderText $ txt ++ "\n\n"

-- Standardize length (add spaces so every string is of same length)
stdLen :: [String] -> ([String], Int)
stdLen sx = do
    let lens = map length sx
    let mxlen = maximum lens
    let toadd = map ((`replicate` ' ') . (mxlen -)) lens
    (zipWith (++) sx toadd, mxlen)

hline :: String -> Int -> String
hline line len = concat (replicate len line)

hTblLine :: String -> String -> String -> String -> [Int] -> String
hTblLine l r sep line lens = l ++ foldr ((\x y -> x ++ sep ++ y) . hline line) "" (init lens) ++ hline line (last lens) ++ r

tblTopLine :: [Int] -> String
tblTopLine = hTblLine "┌─" "─┐" "─┬─" "─"

tblBtmLine :: [Int] -> String
tblBtmLine = hTblLine "└─" "─┘" "─┴─" "─"

tblMidLine :: [Int] -> String
tblMidLine = hTblLine "├─" "─┤" "─┼─" "─"

mkTblRow :: [String] -> String
mkTblRow row = "│ " ++ concatMap (++ " │ ") (init row) ++ last row ++ " │"

mkTbl :: [[String]] -> [Int] -> String
mkTbl [] _ = ""
mkTbl rows lens = do
    let txt = foldr1 (\x y -> x ++ "\n" ++ tblMidLine lens ++ "\n" ++ y) (map mkTblRow rows)
    tblTopLine lens ++ "\n" ++ txt ++ "\n" ++ tblBtmLine lens ++ "\n"


transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

--mkRow :: [String] -> String

{- Map rendering logic -}

-- Render Map
renderMap :: Map -> MapPos -> IO ()
renderMap mp cr = do
    renderMapLoop mp (0,0) cr
    resetColor
    hFlush stdout


-- Recursive loop to render map (map -> current pos -> cursor pos -> IO)
renderMapLoop :: Map -> MapPos -> MapPos -> IO ()

renderMapLoop [] _ _ = putStr ""

renderMapLoop ([]:cls) (_,y) cr = do                -- when one row of map is finished rendering
    putStrLn ""
    renderMapLoop cls (0,y+1) cr

renderMapLoop ((cl:clr):cls) (x,y) (cx,cy) = do     -- when actual tile rendering is happenging
    renderMapTile cl (x == cx && y == cy)
    renderMapLoop (clr:cls) (x+1,y) (cx,cy)


-- Render Map Tile (map tile -> to render cursor -> IO)
renderMapTile :: MapTile -> Bool -> IO ()
renderMapTile tile cr = do
    setColor tile
    let str = strForTile tile
    if cr then setSGR [SetColor Background Vivid Black] else pure ()    --EXP
    --putStr $ if cr then "  " else str                        --EXP
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

-- Fire
setColor (_, _, _, Fire _ _) = do
    setSGR [SetColor Background Vivid Red]
    setSGR [SetColor Foreground Vivid Yellow]

-- Crop
setColor (_, Crop _, _, NoEffect) = do
    setSGR [SetColor Background Vivid Yellow]
    setSGR [SetColor Foreground Dull Black]

-- House
setColor (_ , House _, _, NoEffect) = do
    setSGR [SetColor Background Dull Black]
    setSGR [SetColor Foreground Vivid White]

-- Arable Land
setColor (Land Arable, _, _, NoEffect) = do
    setSGR [SetColor Background Dull Green]
    setSGR [SetColor Foreground Dull Green]

-- Non Arable Land
setColor (Land NonArable, _, _, NoEffect) = do
    setSGR [SetColor Background Dull Yellow]
    setSGR [SetColor Foreground Dull Yellow]

-- Fresh Water
setColor (Water Fresh, _, _, NoEffect) = do
    setSGR [SetColor Background Vivid Cyan]
    setSGR [SetColor Foreground Vivid Cyan]

-- Salty Water
setColor (Water Salty, _, _, NoEffect) = do
    setSGR [SetColor Background Vivid Blue]
    setSGR [SetColor Foreground Vivid Blue]

-- catch-all
setColor _ = resetColor


{- Character Mappings -}
strForTile :: MapTile -> String

-- Crop
strForTile (_, _, _, Fire x _)
        | x == 1 || x == fireMxLvl       = ",,"
        | x == 2 || x == fireMxLvl - 1 = "vv"
        | otherwise = "MM"
strForTile (_, Crop x, _, _)
        | x == 0    = ";;"
        | x < 5     = "ii"
        | otherwise = "ll"
-- House
strForTile (_, House _, _, _) = "⌂⌂"
-- catch-all
strForTile _ = "  "
