-- Advent of Code 2022 - Day 23 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of empty ground tiles is:                 4254
-- The number of the first round where no Elf moves is:  992
--
-- The code is slow, it take > 20 minutes to run part2
-- (Print hard coded values in output)
--
-- (cl) by Arno Jacobs, 2022-12-23

-- 
module AoC2022d23ab where

type Position = (Int,Int)
type Elves = [Position]

filename :: String
filename = "data/inputDay23_2022.txt" 

cElf    = '#' :: Char
cDot    = '.' :: Char

directions :: [[Position]]
directions =   [ [(-1,-1),( 0,-1),( 1,-1)]
                ,[(-1, 1),( 0, 1),( 1, 1)]
                ,[(-1,-1),(-1, 0),(-1, 1)] 
                ,[( 1,-1),( 1, 0),( 1, 1)] ]

addPositions :: Position -> Position -> Position
addPositions (x1,y1) (x2,y2) = (x1+x2,y1+y2)

parse :: [String] -> Elves
parse lines = [ (x,y) | (line,y)  <- zip lines [0..], 
                        (local,x) <- zip line  [0..], 
                        local == cElf ]

empty :: Elves -> Int
empty elves = (1 + maximum cXl - minimum cXl) * (1 + maximum cYl - minimum cYl) - length elves
    where (cXl,cYl)   = (map fst elves,map snd elves)


{- In this order:
    If NO Elf in the N, NE, OR NW adjacent positions, Elf proposes move north 1 step.
    If NO Elf in the S, SE, OR SW adjacent positions, Elf proposes move south 1 step.
    If NO Elf in the W, NW, OR SW adjacent positions, Elf proposes move west  1 step.
    If NO Elf in the E, NE, OR SE adjacent positions, Elf proposes move east  1 step. 
-}
propose :: Elves -> [[Position]] -> Position -> Position
propose elves directions elf
    | allFree   elf elves                   = elf
    | checkMove elf elves (directions !! 0) = addPositions elf (directions !! 0 !! 1)
    | checkMove elf elves (directions !! 1) = addPositions elf (directions !! 1 !! 1)
    | checkMove elf elves (directions !! 2) = addPositions elf (directions !! 2 !! 1)
    | checkMove elf elves (directions !! 3) = addPositions elf (directions !! 3 !! 1)
    | otherwise                             = elf

-- first half of the round
-- Checkis all 8 positions are free - if so - the do nothing - don't move - stay put -- freeze . . .
allFree :: Position -> Elves -> Bool
allFree elf elves = and [ not (elem adjPos elves) |     
                            dx <- [-1,0,1], 
                            dy <- [-1,0,1], 
                            not ((dx == 0) && (dy == 0)),
                            let adjPos = addPositions elf (dx,dy) ] 

checkMove :: Position -> Elves -> [Position] -> Bool
checkMove elf elves deltas = and [ not (elem (addPositions elf delta) elves) | delta <- deltas ]

checkOverlap:: Elves -> Elves -> Elves
checkOverlap propositions elves =
    [ ne |  (p,ix) <- zip propositions [0..], 
            let pc = length (filter (==p) propositions),
            let ne = if pc > 1  then elves !! ix
                                else propositions !! ix ]

oneRound :: Elves -> [[Position]] -> (Elves,[[Position]])
oneRound elves directions = (newRound,newDirections)
        where 
            newRound        = checkOverlap (map (propose elves directions) elves) elves
            newDirections   = turnDirections directions

turnDirections :: [[Position]] -> [[Position]]
turnDirections directions = tail directions ++ [head directions]

workPart1 :: [[Position]] -> Elves -> Int -> Elves
workPart1 _          elves 0        = elves
workPart1 directions elves round    = 
    workPart1 newDirections movedElves (round-1) 
        where
            (movedElves,newDirections) = oneRound elves directions

workPart2 :: [[Position]] -> Elves -> Int -> Int
workPart2 directions elves rounds   
    | movedElves == elves   = rounds+1
    | otherwise             = workPart2 newDirections movedElves (rounds+1)
        where
            (movedElves,newDirections) = oneRound elves directions

-- Extra pretty print for Elves on the ground . . . ------------------------------
--
showElves :: Elves -> IO ()
showElves = showLines . matchSpritesWithElves

-- Check if pixel at column X matches the clock cycle
-- If so draw 'pixel' - otherwise leave blank 
matchSpritesWithElves :: Elves -> [String]
matchSpritesWithElves elves = [[  pixel elves (x,y) | x <- [minX..maxX] ] 
                                                    | y <- [minY..maxY] ] 
    where
        (cXl,cYl)   = (map fst elves,map snd elves)
        minX = minimum cXl
        maxX = maximum cXl
        minY = minimum cYl
        maxY = maximum cYl
        pixel elves point   | elem point elves  = cElf
                            | otherwise         = cDot

-- 'pretty print' the lines
showLines :: [String] -> IO ()
showLines []     = return ()
showLines (l:ls) = do  putStrLn l
                       showLines ls


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 23  (Haskell)"
            elves <- parse <$> lines <$> readFile filename
            putStr "The number of empty ground tiles is:                 "
            -- print $ empty $ workPart1 directions elves 10
            putStrLn "4254 *"
            putStr "The number of the first round where no Elf moves is:  "
            -- print $ workPart2 directions elves 0 -- slow
            putStrLn "992 *"
            putStrLn "0K.\n"

            -- showElves elves      -- for example
