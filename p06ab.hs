-- Advent of Code 2022 - Day 6 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Start of message marker for part 1 is at: 1766
-- Start of message marker for part 2 is at: 2383
--
-- (cl) by Arno Jacobs, 2022-12-06

module AoC2022d06ab where

import Data.List (nub)

filename :: String
filename = "data/inputDay06_2022.txt"

-- Recursively testing for unique elements
messageMarker :: Int -> String -> Int
messageMarker = marker 0 
    where
        marker m wl ds  | length ds < wl    = (-1)      -- if no start position is found
                        | nub ps == ps      = m + wl    -- all 'wl' characters are unique
                        | otherwise         = marker (m+1) wl $ tail ds
            where ps = take wl ds

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 6  (Haskell)"
            day6 <- readFile filename
            putStr   "Start of message marker for part 1 is at: "
            print $ messageMarker  4 day6
            putStr   "Start of message marker for part 2 is at: "
            print $ messageMarker 14 day6
            putStrLn "0K.\n"
