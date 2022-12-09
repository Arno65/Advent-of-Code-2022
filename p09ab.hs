-- Advent of Code 2022 - Day 9 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- With  2 knots the number of positions the tail visited is: 6023
-- With 10 knots the number of positions the tail visited is: 2533
-- 
-- (cl) by Arno Jacobs, 2022-12-09

module AoC2022d09ab where

import Data.List
  
filename :: String
filename = "data/inputDay09_2022.txt"

type Steps      = Int
type Direction  = (Int,Int)
type Position   = (Int,Int)
type Positions  = [Position]
type Motion     = (Direction,Steps)
type Motions    = [Motion]

knotsPart1, knotsPart2 :: Int
knotsPart1 = 2
knotsPart2 = 10

startPosition :: Position
startPosition = (0,0)

splitLines :: [String] -> Motions
splitLines = map splitLine
    where
        splitLine xs = ((readDirection . head) xs, (read . drop 2) xs)
        readDirection 'U' = ( 0, 1)     -- up
        readDirection 'R' = ( 1, 0)     -- right   
        readDirection 'D' = ( 0,-1)     -- down
        readDirection 'L' = (-1, 0)     -- left
        readDirection '_' = ( 0, 0)     -- none

-- Part 1 & 2

-- Walk the 'head' and let all knots follow its path, step by step, knot by knot
knotPath :: Int -> Motions -> Positions
knotPath nk = knotPath' startPosition startKnotPositions 
    where
        startKnotPositions = replicate (nk-1) startPosition
        knotPath' _  _   []         = []
        knotPath' ph pks (ds:dss)   = tailSteps ++ knotPath' nph npks dss
            where 
                (tailSteps,(nph,npks)) = stepKnotsAfterHead ph pks ds []

-- Work step after step, knot after knot, for the 'head', 
-- let the 'tail', the last knot follow its steps
-- Record all and only the positions the 'tail' visited                
stepKnotsAfterHead :: Position -> Positions ->  Motion -> Positions -> 
                        (Positions,(Position,Positions))        
stepKnotsAfterHead cph cpks (d,s) tLastPoss 
    | s == 0    = (tLastPoss,(cph,cpks))
    | otherwise = stepKnotsAfterHead nph npks (d,(s-1)) (tLastPoss ++ [last npks])
        where
            nph     = (fst cph + fst d, snd cph + snd d) 
            npks    = knotsToknotsToHead nph cpks
            knotsToknotsToHead hp []        = [] 
            knotsToknotsToHead hp (kp:kps)  = [nkp] ++ knotsToknotsToHead nkp kps
                where
                    nkp = tailToHead hp kp 
        
-- The important "follow that knot" function
-- get the ONE step for a 'tail' knot to move to its 'head' knot
-- Including 'special' rules for a diagonal move
tailToHead :: Position -> Position -> Position
tailToHead (hx,hy) (tx,ty)  | isDiagonal    = diagonalStep (hx,hy) (tx,ty)
                            | otherwise     = (nextTx,nextTy)
    where 
        isDiagonal  = (hx /= tx) && (hy /= ty)
        diagonalStep (hx,hy) (tx,ty) 
            |  abs (hx-tx) == 1 
            && abs (hy-ty) == 1     = (tx,ty)
            |  hx > tx && hy > ty   = ( tx + 1, ty + 1 )
            |  hx > tx && hy < ty   = ( tx + 1, ty - 1 )
            |  hx < tx && hy > ty   = ( tx - 1, ty + 1 )
            |  otherwise            = ( tx - 1, ty - 1 )
        nextTx      = tx + oneStep (tx - hx)
        nextTy      = ty + oneStep (ty - hy)
        oneStep ds  | ds > 1    = -1
                    | ds < (-1) = 1
                    | otherwise = 0

-- remove all 'double++' positions from list and return its length
countTailPositions :: Positions -> Steps
countTailPositions = length . nub 


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 9  (Haskell)"
            day9 <- splitLines <$> lines <$> readFile filename
            putStr "With  "
            putStr $ show knotsPart1
            putStr " knots the number of positions the tail visited is: "
            print $ countTailPositions $ knotPath knotsPart1 day9   -- ONE Step Beyond !  
            putStr "With "
            putStr $ show knotsPart2
            putStr " knots the number of positions the tail visited is: "
            print $ countTailPositions $ knotPath knotsPart2 day9   -- nine steps beyond
            putStrLn "0K.\n"

