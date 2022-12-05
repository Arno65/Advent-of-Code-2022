-- Advent of Code 2022 - Day 5 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The top crates after all CrateMover 9000 moves are: SHQWSRBDL
-- The top crates after all CrateMover 9001 moves are: CDTQZHBRS
--
-- (cl) by Arno Jacobs, 2022-12-05

module AoC2022d05ab where

import Data.List        (transpose)
import Data.List.Split  (splitOn)

filename :: String
filename = "data/inputDay05_2022.txt"

-- First part 'parsing' the data and reformatting it
-- 'parse' by fixed format
crateAtPile :: String -> String
crateAtPile cx = [ cx !! pile | pile <- [1,5..(length cx - 1)]]

-- Create list of Strings with crates per pile
getCrates :: [String] -> [String]
getCrates = map stackCrates . transpose . map crateAtPile . takeWhile (/= "") 
    where stackCrates = takeWhile (/= ' ') . tail . reverse

-- 'parse' by fixed format
-- renumber to correct index as in: 
-- first pile has index 1 in input and 0 in Haskell code
moveNumberCratesFromTo :: String -> (Int,(Int,Int))
moveNumberCratesFromTo mx = 
    (read (parts !! 1), ( read (parts !! 3) - 1, read (parts !! 5) - 1)) 
        where parts = splitOn " " mx

-- parse moves 
-- "move 3 from 1 to 7"  to  (3,(0,6))
getMoves :: [String] -> [(Int,(Int,Int))]
getMoves = map moveNumberCratesFromTo . tail . dropWhile (/= "")

-- Part 1 & 2

-- Work all moves
workMovesOnCratePiles :: Int -> [(Int,(Int,Int))] -> [String] -> [String]
workMovesOnCratePiles cm []              cps = cps
workMovesOnCratePiles cm ((n,(f,t)):rms) cps = workMovesOnCratePiles cm rms ncps 
    where
        ncps = [ newCratePile cp nfc ntc ix f t | (cp,ix) <- zip cps [0..]]
        rfc = reverse $ cps !! f
        tfc = take n rfc
        pfc = if cm == 9000 then tfc else reverse tfc
        nfc = reverse $ drop n rfc
        ntc = (cps !! t) ++ pfc
        newCratePile cp nfc ntc ix f t  
            | ix == f   = nfc
            | ix == t   = ntc
            | otherwise = cp

-- get top crates
toCrates :: [String] -> String
toCrates = map last . map (\c -> "." ++ c) 

workToTopCrates :: [String] -> Int -> String
workToTopCrates xs cm = toCrates $ workMovesOnCratePiles cm moves crates
    where 
        crates = getCrates xs
        moves  = getMoves xs


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 5  (Haskell)"
            day5 <- lines <$> readFile filename
            putStr   "The top crates after all CrateMover 9000 moves are: "
            putStrLn $ workToTopCrates day5 9000
            putStr   "The top crates after all CrateMover 9001 moves are: "
            putStrLn $ workToTopCrates day5 9001
            
            putStrLn "0K.\n"

            


