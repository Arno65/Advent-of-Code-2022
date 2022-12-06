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

part1 =  4 :: Int
part2 = 14 :: Int

marker :: Int -> Int -> String -> Int
marker m wl (c:ds)  | length tss == wl  = m + wl
                    | length ds < wl    = (-1)
                    | otherwise         = marker (m+1) wl ds
    where tss = nub $ [c] ++ take (wl-1) ds

startMessage :: Int -> String -> Int
startMessage wl = marker 0 wl  

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 6  (Haskell)"
            day6 <- readFile filename
            putStr   "Start of message marker for part 1 is at: "
            print $ startMessage part1 day6
            putStr   "Start of message marker for part 2 is at: "
            print $ startMessage part2 day6
            putStrLn "0K.\n"
