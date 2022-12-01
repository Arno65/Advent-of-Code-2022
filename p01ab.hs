-- Advent of Code 2022 - Day 1 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The Elf carrying the most Calories, has a total of:  70116
-- The three Elves carrying the most Calories, have a total of: 206582
--
-- (cl) by Arno Jacobs, 2022-12-01

-- 
module AoC2022d01ab where

filename = "data/inputDay01_2022.txt"

calories :: [String] -> [Int]
calories []      = []
calories ([]:xs) = calories xs
calories xs      = [elve_calories xs] ++ calories (elves_calories xs)
    where
        elve_calories   = sum . map read . takeWhile (/="")
        elves_calories  = tail . dropWhile (=="") 

topSum :: [Int] -> Int -> Int
topSum _ 0 = 0
topSum xs c = mx + topSum (removeOne xs mx) (c-1)
    where
        mx  = maximum xs

-- Only remove ONE score per top selection
-- It's NOT the case in my dataset BUT . . .
-- It could be that top calory score's are equal for some elves
removeOne :: [Int] -> Int -> [Int]
removeOne xs mx = takeWhile (/= mx) xs ++ tail (dropWhile (/= mx) xs)

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 1 - both parts in Haskell"
            day1 <- calories <$> lines <$> readFile filename

            putStr   "The Elf carrying the most Calories, has a total of: "
            print $ maximum day1
            putStr   "The three Elves carrying the most Calories, have a total of: "
            print $ topSum day1 3
            putStrLn "0K.\n"

            
