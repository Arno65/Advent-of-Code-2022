-- Advent of Code 2022 - Day 10 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of the six signal strengths is: 14720
-- The text on the CRT is: FZBPBFZF
-- 
-- (cl) by Arno Jacobs, 2022-12-10

module AoC2022d10ab where

import Data.Char (isNumber)
  
filename :: String
filename = "data/inputDay10_2022.txt"


-- For part 1 ~ Find the signal strength DURING the
--              20th, 60th, 100th, 140th, 180th, and 220th cycles.
measureDuringCycles :: [Int]
measureDuringCycles = [20,60,100,140,180,220]

-- Parse the input code and create list of register X at every clock cycle
parseCode :: [String] -> [Int]
parseCode = parseCode' 1 1
    where
        parseCode' _ _ []       = []
        parseCode' c x (i:ril)
            | i == "noop"   = [x]   ++ parseCode' (c+1)  x             ril
            | otherwise     = [x,x] ++ parseCode' (c+2) (x + getInt i) ril

-- Get a 'standard' Int from a arbitrary String (safe)
-- Pick the first positive or negate integer from a alphanum~string
getInt :: String -> Int
getInt []       = 0
getInt (c:[])   | isNumber c    = read [c]
                | otherwise     = 0
getInt (c:cs)   | isNumber c                        =    read $ takeWhile isNumber (c:cs)
                | c == '-' && isNumber (head cs)    = - (read $ takeWhile isNumber    cs)
                | otherwise                         = getInt cs

-- Part 1
-- Read X at the measure times and work the given rules
testRunPart1 :: [Int] -> Int
testRunPart1 cl = sum [ c * x | c <- measureDuringCycles, let x = cl !! (c-1)]

-- Part 2

showCRT :: [Int] -> IO ()
showCRT = showLines . matchSpritesWithX

-- Check if pixel at column X matches the clock cycle
-- If so draw 'pixel' - otherwise leave blank 
matchSpritesWithX :: [Int] -> [String]
matchSpritesWithX cl = [[  getPixel x (x+y*40) cl | x <- [0..39] ] | y <- [0..5]] 
    where
        getPixel :: Int -> Int -> [Int] -> Char
        getPixel sc pos cl  |  cl !! pos > sc - 2
                            && cl !! pos < sc + 2   = '#'
                            | otherwise             = ' '

-- 'pretty print' the CRT lines
showLines :: [String] -> IO ()
showLines []     = return ()
showLines (l:ls) = do  putStrLn l
                       showLines ls


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 10  (Haskell)"
            day10 <- parseCode <$> lines <$> readFile filename
            putStr "The sum of the six signal strengths is: "
            print $ testRunPart1 day10
            putStrLn "The CRT image after running the code is:\n"
            showCRT day10
            putStrLn "\n0K.\n"

