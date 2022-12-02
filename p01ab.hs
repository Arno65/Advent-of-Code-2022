-- Advent of Code 2022 - Day 1 task A & B
-- Solutions in Haskell and Lisp
-- (Ter leering ende vermaeck...)
--
-- The Elf carrying the most Calories, has a total of:  70116
-- The three Elves carrying the most Calories, have a total of: 206582
--
-- (cl) by Arno Jacobs, 2022-12-01

-- 
module AoC2022d01ab where

import Data.List

filename :: String
filename = "data/inputDay01_2022.txt"

calories :: [String] -> [Int]
calories []      = []
calories ([]:xs) = calories xs
calories xs      = [elve_calories xs] ++ calories (rest_calories xs)
    where
        elve_calories = sum . map read . takeWhile (/="")
        rest_calories = dropWhile (/="") 

maximumN :: Int -> [Int] -> Int
maximumN n = sum . take n . reverse . sort

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 1  (Haskell)"
            day1 <- calories <$> lines <$> readFile filename

            putStr   "The Elf carrying the most Calories,         has  a total of:  "
            print $ maximum day1
            putStr   "The three Elves carrying the most Calories, have a total of: "
            print $ maximumN 3 day1 
            putStrLn "0K.\n"
