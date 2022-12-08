-- Advent of Code 2022 - Day 8 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of trees visible from outside the grid is:   1818
-- The highest scenic score possible for any tree is:    368368
--
-- (cl) by Arno Jacobs, 2022-12-08

module AoC2022d08ab where

import Data.Char
import Data.List

filename :: String
filename = "data/inputDay08_2022.txt"

-- Convert the list of Strings into a grid of Ints.
convert :: [String] -> [[Int]]
convert = map (map readChar)
    where 
        readChar c  | isNumber c    = read [c]
                    | otherwise     = (-1)

-- Part 1

-- Is a tree visible from any direction.
isVisible :: (Int,Int) -> [[Int]] -> Bool
isVisible (c,r) grid    =  (height > mhl) || (height > mhr) 
                        || (height > mhu) || (height > mhd)
    where 
        height  = grid !! r !! c
        -- rows
        row     = grid !! r
        left    = take c row
        right   = drop (c+1) row
        mhl     = maximum left
        mhr     = maximum right
        -- columns
        tgrid   = transpose grid
        column  = tgrid !! c
        up      = take r column
        down    = drop (r+1) column
        mhu     = maximum up
        mhd     = maximum down

-- Check the interior trees of the forrest, a grid of trees.
innerVisible :: [[Int]] -> [(Int,Int)]
innerVisible grid = 
    [ (x,y) | x <- [1..cx], y <- [1..cy], isVisible (x,y) grid ]
        where 
            cy = length grid - 2
            cx = length (head grid) -2 

-- Counting all visible trees. All the trees on the edge of the grid are visible.
-- Next count all visible interior trees.
countVisibleTrees :: [[Int]] -> Int
countVisibleTrees []    = 0
countVisibleTrees grid  = 2 * (length grid) +
                          2 * (length (head grid) - 2) + 
                          length (innerVisible grid)
    
-- Part 2

-- Get the score for one direction ( left, right, up, down )
getScore :: [Int] -> Int -> Int
getScore [] _   = 0
getScore xs h   = getScore' xs h 0
    where
        getScore' []     h s    = s
        getScore' (x:xs) h s    | x < h     = getScore' xs h (s+1) 
                                | otherwise = s+1

-- Get the scenic score per tree.
scenicScore :: (Int,Int) -> [[Int]] -> Int
scenicScore (c,r) grid = sl * sr * st * sb
    where 
        height  = grid !! r !! c
        -- rows
        row     = grid !! r
        left    = reverse $ take c row
        right   = drop (c+1) row
        sl      = getScore left  height
        sr      = getScore right height
        -- columns
        tgrid   = transpose grid
        column  = tgrid !! c
        top     = reverse $ take r column
        bottom  = drop (r+1) column
        st      = getScore top    height
        sb      = getScore bottom height

-- All trees on the edge have at least on direction with score 0.
-- So the product, te scenic score will be 0 for all trees on the edge.
-- Next, get the scenic score for every interior tree.
-- Pick the maximum score and return it.
highestScenicScore :: [[Int]] -> Int
highestScenicScore grid = maximum
    [ scenicScore (x,y) grid | x <- [1..cx], y <- [1..cy] ]
        where 
            cy = length grid - 2
            cx = length (head grid) -2 


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 8  (Haskell)"
            day8 <- convert <$> lines <$> readFile filename
            putStr "The number of trees visible from outside the grid is:   "
            print $ countVisibleTrees day8
            putStr "The highest scenic score possible for any tree is:    "
            print $ highestScenicScore day8 
            putStrLn "0K.\n"
