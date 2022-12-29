-- Advent of Code 2022 - Day 18 task A only
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The surface area of the scanned lava droplet is: 3326
-- 
-- Only a neat solution for the first part 
--
-- (cl) by Arno Jacobs, 2022-12-18

-- 
module AoC2022d18a where

import Data.List.Split  (splitOn)

type Cube   = (Int,Int,Int)
type Cubes  = [Cube]
  
filename :: String
filename = "data/inputDay18_2022.txt"

deltas :: Cubes
deltas = [(-1,0,0),(1,0,0),(0,-1,0),(0,1,0),(0,0,-1),(0,0,1)]

parse :: [String] -> Cubes
parse = map ((\(e1:e2:e3:_) -> (e1,e2,e3)) . map read . splitOn ",") 

sumCubes :: Cube -> Cube -> Cube
sumCubes (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

neighbours :: Cube -> Cubes -> Cubes
neighbours cube cubes = concat [ filter (==neighbour) cubes |   
                                 delta <- deltas,
                                 let neighbour = sumCubes cube delta ] 

surfaceArea :: Cubes -> Int
surfaceArea cubes = surfaceArea' (6 * length cubes) cubes cubes
    where
        surfaceArea' area _     []            = area
        surfaceArea' area cubes (cube:rcubes) = surfaceArea' (area - overlap) cubes rcubes    
            where 
                overlap = length $ neighbours cube cubes


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 18  (Haskell) First part only"
            cubes <- parse <$> lines <$> readFile filename
            putStr "The surface area of the scanned lava droplet is: "
            print $ surfaceArea cubes   
            putStrLn "\n0K.\n"

