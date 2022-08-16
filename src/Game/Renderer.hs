module Game.Renderer
( renderMap )
where

import System.Console.ANSI
import Game.Map ( MapCell )
import System.IO (stdout, hFlush)


renderMap :: [[MapCell]] -> IO ()
renderMap mp = do
    clearScreen
    setCursorPosition 0 0
    let n = head (head mp)
    putStrLn $ mapToStr mp
    setSGR [SetColor Foreground Vivid Yellow]
    putStr $ replicate (round (fromIntegral n / 5)) '='
    putStr $ "[ " ++ show n ++ " ]"
    putStr $ replicate (round (fromIntegral n / 5)) '='
    putStr ">"
    setSGR [Reset]
    hFlush stdout

mapToStr :: [[MapCell]] -> String
mapToStr = foldl (\s r -> s ++ "\n" ++ mapRowToStr r) ""


mapRowToStr :: [MapCell] -> String
mapRowToStr r = init $ concat [show x ++ " " | x <- r]
