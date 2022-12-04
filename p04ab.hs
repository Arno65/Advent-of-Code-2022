-- Advent of Code 2022 - Day 4 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of assignment pairs that  fully  overlap is: 459
-- The number of assignment pairs that somehow overlap is: 779
--
-- (cl) by Arno Jacobs, 2022-12-04

module AoC2022d04ab where

import Data.List.Split (splitOn)

filename :: String
filename = "data/inputDay04_2022.txt"

-- Split an input line into a pair of pairs
-- No save head or last, working 0K. here and now
sections :: String -> ((Int,Int),(Int,Int))
sections se = ((read (head lp),read (last lp)),(read (head rp),read (last rp)))
    where 
        sp = splitOn "," se
        lp = splitOn "-" (head sp) 
        rp = splitOn "-" (last sp)

-- Part 1
-- Check that all sections of the left pair fully overlap with 
-- the right pair, and vice versa.
fullyOverlap :: ((Int,Int),(Int,Int)) -> Bool
fullyOverlap ((l1,r1),(l2,r2))  =  (l1 >= l2 && r1 <= r2) 
                                || (l2 >= l1 && r2 <= r1)
    
-- Filter and count the fully overlapping pairs    
countFullyOverlap :: [((Int,Int),(Int,Int))] -> Int
countFullyOverlap = length . filter fullyOverlap

-- Part 2
-- Check if any sections of the left pair overlap with the right pair.
-- 'Lazy' As soon as there is one overlapping section there is a hit.
overlap :: ((Int,Int),(Int,Int)) -> Bool
overlap ((l1,r1),(l2,r2)) = [ s | s <- [l1..r1], elem s [l2..r2]] /= []

-- Filter and count the pairs with any overlapping section    
countOverlaps :: [((Int,Int),(Int,Int))] -> Int
countOverlaps = length . filter overlap


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 4  (Haskell)"
            day4 <- map sections <$> lines <$> readFile filename
            putStr   "The number of assignment pairs that  fully  overlap is: "
            print $ countFullyOverlap day4
            putStr   "The number of assignment pairs that somehow overlap is: "
            print $ countOverlaps day4            
            putStrLn "0K.\n"
