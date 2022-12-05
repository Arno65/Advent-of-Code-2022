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

data CraneMover = CraneMover_9000 | CraneMover_9001 deriving (Eq, Show)

type Crate          = Char
type CratePile      = [Crate]
type FromToIndex    = (Int,Int)
type CrateMoves     = (Int,FromToIndex)

-- First part 'parsing' the data and reformatting it
-- 'parse' by fixed format
-- From: "[Z] [M] [P]"  to: "ZMP"
cratesAtPile :: String -> CratePile
cratesAtPile cx = [ cx !! pile | pile <- [1,5..(length cx - 1)]]

-- Create list of Strings with crates per pile
-- Take the input up to the blank line
getCrates :: [String] -> [CratePile]
getCrates = map stackCrates . transpose . map cratesAtPile . takeWhile (/= "") 
    where stackCrates = takeWhile (/= ' ') . tail . reverse

{-
From:
        [D]    
    [N] [C]    
    [Z] [M] [P]
    1   2   3 

to: (map cratesAtPile)      to: (transpose)     to: (map stackCrates)
    " D "                   " NZ1"              "ZN"
    "NC "                   "DCM2"              "MCD"
    "ZMP"                   "  P3"              "P"
    "123"
-}

-- 'parse' by fixed format
-- renumber to correct index as in: 
-- first pile has index 1 in input and 0 in Haskell code
-- From:                    to: (splitOn)                       to (read)
-- "move 3 from 1 to 7"     ["move","3","from","1","to","7"]    (3,(0,6))
moveNumberCratesFromTo :: String -> CrateMoves
moveNumberCratesFromTo mx = 
    (read (parts !! 1), ( read (parts !! 3) - 1, read (parts !! 5) - 1)) 
        where parts = splitOn " " mx

-- parse moves 
-- Take the input after the blank line
-- From: "move 3 from 1 to 7"  to: (3,(0,6))
getMoves :: [String] -> [CrateMoves]
getMoves = map moveNumberCratesFromTo . tail . dropWhile (/= "")

-- Part 1 & 2

-- Work all moves
workMovesOnCratePiles :: CraneMover -> [CrateMoves] -> [CratePile] -> [CratePile]
workMovesOnCratePiles cm []              cps = cps
workMovesOnCratePiles cm ((n,(f,t)):rms) cps = workMovesOnCratePiles cm rms ncps 
    where
        -- Create new crate piles
        ncps = [ newCratePile cp nfc ntc ix f t | (cp,ix) <- zip cps [0..]]
        rfc = reverse $ cps !! f    -- Take the reversed crate pile at index 'from'
        tfc = take n rfc            -- Take last the number 'n' of piles
        -- Do the Crane Mover check - reverse the taken piles back if needed
        pfc = if cm == CraneMover_9000 then tfc else reverse tfc 
        ntc = (cps !! t) ++ pfc     -- add the 'from' crates on top of the 'to' pile 
        -- Drop the taken crates from the 'from' pile and reverse back to original order
        nfc = reverse $ drop n rfc
        -- Helper function for setting the new 'from' and 'to' piles
        newCratePile cp nfc ntc ix f t  
            | ix == f   = nfc
            | ix == t   = ntc
            | otherwise = cp

-- Get the list of top crates
-- If a pile has NO crates a '.' will be placed at that location
toCrates :: [CratePile] -> [Crate]
toCrates = map last . map (\c -> "." ++ c) 

-- Do the work, first seperate CratePile data from FromToindex list
-- Work all moves from the FromToIndex list 
-- Get and return the top crates from the worked CratePile data
workToTopCrates :: [String] -> CraneMover -> [Crate]
workToTopCrates xs cm = toCrates $ workMovesOnCratePiles cm moves crates
    where 
        crates = getCrates xs
        moves  = getMoves xs


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 5  (Haskell)"
            day5 <- lines <$> readFile filename
            putStr   "The top crates after all CrateMover 9000 moves are: "
            putStrLn $ workToTopCrates day5 CraneMover_9000
            putStr   "The top crates after all CrateMover 9001 moves are: "
            putStrLn $ workToTopCrates day5 CraneMover_9001
            putStrLn "0K.\n"
