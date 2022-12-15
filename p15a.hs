-- Advent of Code 2022 - Day 15 task A only
-- Solution in Haskell
-- (Ter leering ende vermaeck...)
--
-- Quested number of positions: 4737567
--
-- This code is very slow, even very^very slow for part 2 
-- So for now only part 1 
-- 
-- (cl) by Arno Jacobs, 2022-12-15

-- module AoC2022d15a where
  
import Data.Char

filename :: String
filename = "data/inputDay15_2022.txt"

-- scanline for part 1
scanLine = 2000000 :: Int

type Position   = (Int,Int)

-- Manhattan distance
manhattan :: Position -> Position -> Int
manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- get range of the space based on all coordiantes of beacons and sensors
-- (both added into one list)
getRange :: [Position] -> (Position,Position)
getRange (p:ps) = getRange' ps (p,p)
    where
        getRange' [] minmaxp = minmaxp 
        getRange' ((px,py):ps) ((minX,minY),(maxX,maxY))    
            = getRange' ps ((nMinX,nMinY),(nMaxX,nMaxY))    
                where
                    nMinX = min px minX
                    nMinY = min py minY
                    nMaxX = max px maxX
                    nMaxY = max py maxY

-- Get list of positive (only) Ints from an arbitrary String (safe)
readAllZs :: String -> [Int]
readAllZs []      = []
readAllZs (c:cs)
    | c == '-'      = [- read (takeWhile isNumber (cs))] ++ nxt
    | isNumber c    = [read (takeWhile isNumber (c:cs))] ++ nxt
    | otherwise     = readAllZs cs
        where 
            nxt = readAllZs $ dropWhile isNumber cs

parse :: String -> (Position,Position)
parse pl = ((sx,sy),(bx,by)) where (sx:sy:bx:by:_) = readAllZs pl

-- Function for part 1 - just slow
noBeaconsInRow :: Int ->  [(Position,Position)] -> Int
noBeaconsInRow row sbpl =  sum [ 1 |    x <- [minX..maxX], 
                                        not (elem (x,row) bl),
                                        not (beaconsOnPosition (x,row) smdl) ]
     where 
        sl = map fst sbpl
        bl = map snd sbpl
        ((minRX,_),(maxRX,_)) = getRange $ sl ++ bl
        smdl = map scannerAndDistance sbpl
        scannerAndDistance (sxy,bxy) = (sxy,manhattan sxy bxy) 
        minX = minRX + minimum [ sx - md | ((sx,_),md) <- smdl ]
        maxX = maxRX + maximum [ sx + md | ((sx,_),md) <- smdl ]

-- For both part 1 & 2
beaconsOnPosition :: Position -> [(Position,Int)] -> Bool
beaconsOnPosition _ [] = True
beaconsOnPosition txy ((sxy,md):rsmdl)
    = (tmd > md) && beaconsOnPosition txy rsmdl
        where
            tmd = manhattan txy sxy


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 15 - part 1 only (Haskell)"
            day15 <- map parse <$> lines <$> readFile filename
            putStr "Quested number of positions: "
            print $ noBeaconsInRow scanLine day15
            putStrLn "\n0K.\n"

           