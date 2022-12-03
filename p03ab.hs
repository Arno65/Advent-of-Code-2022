-- Advent of Code 2022 - Day 3 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of the 'rucksack'  priorities of the item types: 7446
-- The sum of the 'Elf group' priorities of the item types: 2646
--
-- (cl) by Arno Jacobs, 2022-12-03

module AoC2022d03ab where

import Data.Char

filename :: String
filename = "data/inputDay03_2022.txt"

-- item priority calculation
ordItem :: Char -> Int
ordItem i   | oi > 96   = oi - 96   -- ord 'a' is 97
            | otherwise = oi - 38   -- 64 - 26
    where oi = ord i

-- Part 1

splitToCompartments :: String -> (String,String)
splitToCompartments xs = (take hsl xs, drop hsl xs)
    where hsl = div (length xs) 2 

-- In case of NO double, ordItem '&' will return 0
prioritizeRucksack :: (String,String) -> Int
prioritizeRucksack (c1,c2) = 
    (ordItem . head) $ [ tc | tc <- c1, elem tc c2 ] ++ "&"

sumRucksackPriorities :: [String] -> Int
sumRucksackPriorities = sum . map (prioritizeRucksack . splitToCompartments) 

-- Part 2

-- 0K. here BUT not 'safe' is r2 or r3 is 'empty'
splitElfGroups :: [String] -> [[String]]
splitElfGroups []               = []    
splitElfGroups (r1:r2:r3:rs)    = [[r1,r2,r3]] ++ splitElfGroups rs 

prioritizeElfGroups :: [String] -> Int
prioritizeElfGroups (r1:r2:r3:_) = 
    (ordItem . head) $ [ tc | tc <- r1, elem tc r2, elem tc r3 ] ++ "&"

sumElfGroupPriorities :: [String] -> Int
sumElfGroupPriorities = sum . map prioritizeElfGroups . splitElfGroups


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 3  (Haskell)"
            day3 <- lines <$> readFile filename
            putStr   "The sum of the 'rucksack'  priorities of the item types: "
            print $ sumRucksackPriorities day3
            putStr   "The sum of the 'Elf group' priorities of the item types: "
            print $ sumElfGroupPriorities day3
            putStrLn "0K.\n"
