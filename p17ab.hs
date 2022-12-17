-- Advent of Code 2022 - Day 17 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The height of the tower for part 1 is:          3065
-- The height of the tower for part 2 is: 1562536022966
-- 
-- Even compiled the code will run for about 80 seconds
-- So hard code output with *-marks
--
-- (cl) by Arno Jacobs, 2022-12-17


-- module AoC2022d17ab where

type Direction  = Char
type Distance   = Int
type Distances  = [Int]
type Directions = [Direction]
type Position   = (Distance,Distance)
type Chamber    = [Position]
type Rock       = [Position]
type Rocks      = [Rock]
  
filename :: String
filename = "data/inputDay17_2022.txt"

-- The number of falling rocks
fallingPart1    =          2022 :: Int
fallingPart2    = 1000000000000 :: Int
-- This number of rocks is enough to do the calculations for part 2
fallingRange    = 3 * fallingPart1      

-- These are the rocks falling - one after the other
-- also in this order of this list
rocks :: Rocks
rocks = [[(0,0),(1,0),(2,0),(3,0)]
        ,[(1,0),(0,1),(1,1),(2,1),(1,2)]
        ,[(0,0),(1,0),(2,0),(2,1),(2,2)]
        ,[(0,0),(0,1),(0,2),(0,3)]
        ,[(0,0),(1,0),(0,1),(1,1)]] 

-- Char direction codes
cLeft   = '<' :: Direction
cRight  = '>' :: Direction


getDirection :: Direction -> Distance
getDirection '<' = (-1)
getDirection _   = 1

-- The width of the chamber
width = 7 :: Distance
-- Each rock appears so that its left edge is two units away from the left wall 
startLeft = 2 :: Distance
-- and its bottom edge is three units above the highest rock in the room 
-- (or the floor, if there isn't one).
startTop = 3 :: Distance

maxHeight :: Chamber -> Distance
maxHeight []        = 0
maxHeight chamber   = (maximum . map snd) chamber

-- Check it the moved rock still fits fine in the chamber
fitInCamber :: Chamber -> Rock -> Bool
fitInCamber chamber rock = free && inChamber    
    where 
        free        = [ 1 | p <- rock, elem p chamber ] == []
        minX        = (minimum (map fst rock)) >= 0
        maxX        = (maximum (map fst rock)) <  width
        minY        = (minimum (map snd rock)) >= 0
        inChamber   = minX && maxX && minY 
        
-- Rock moves -- ONLY if there is space -- the wall will stop (no make) the move
-- ALSO it's not allowed to move over an existing rock    
moveLeft :: Chamber -> Rock -> Rock
moveLeft chamber rock = if fitInCamber chamber movedRock then movedRock else rock
    where
        movedRock = map (\(x,y) -> (x-1,y)) rock 

moveRight :: Chamber -> Rock -> Rock
moveRight chamber rock = if fitInCamber chamber movedRock then movedRock else rock
    where
        movedRock = map (\(x,y) -> (x+1,y)) rock 

moveDown :: Chamber -> Rock -> Rock
moveDown chamber rock = if fitInCamber chamber movedRock then movedRock else rock
    where
        movedRock = map (\(x,y) -> (x,y-1)) rock 

-- The rock is place n-placed above the heighest rock within the camber
-- So no need to check if the rock fits in the chamber
addUp :: Distance -> Rock -> Rock
addUp moveUp rock = map (\(x,y) -> (x,y+moveUp)) rock 


-- Part 1 --- -- ---- - --- -- ---- - --- -- ---- - --- -- ---- - --- -- ---- -
-- Work the falling rock 
-- Save all distances after every dropped and resting rock
towerHeights :: Directions -> Distances
towerHeights = towerHeights' [] [] 0 

-- Dropping rock is NOT done endless here - not a lazy algoritm
-- But - only take what we need - for finding repetition
towerHeights' :: Chamber -> Distances -> Int -> Directions -> Distances
towerHeights' chamber pds rock dirs
    | rock > fallingRange       = pds
    | otherwise                 = towerHeights' newChamber npds (rock+1) nds
        where
            rix = mod rock (length rocks) 
            (newChamber,nds) = dropRock chamber (rocks !! rix) dirs
            npds = pds ++ [1 + maxHeight chamber]

dropRock :: Chamber -> Rock -> Directions -> (Chamber,Directions)
dropRock chamber rock dirs = dropRock' chamber initRock dirs
    where
        ds = if (chamber == []) then 0 else 1 
        -- first place the rock in the chamber
        initRock =  moveRight chamber $ 
                    moveRight chamber $ 
                    addUp (startTop + ds + maxHeight chamber) rock    
            
dropRock' :: Chamber -> Rock -> Directions -> (Chamber,Directions)
dropRock' chamber rock (d:ds)   
    |  rockPushed == rockDropped    = (newChamber,ds)       -- rock fixed AFTER  push
    |  rock       == rockDropped    = (newChamber,(d:ds))   -- rock fixed BEFORE push 
    |  otherwise                    = dropRock' chamber rockDropped ds
        where
            -- first push - if it still fits in the chamber
            rockPushed  = if (d == cLeft) then moveLeft  chamber rock 
                                          else moveRight chamber rock
            -- then fall - if possible...
            rockDropped = moveDown chamber rockPushed
            newChamber  = chamber ++ rockDropped


-- Part 2 --- -- ---- - --- -- ---- - --- -- ---- - --- -- ---- - --- -- ---- -
-- Looking for repetitions in the growth of the tower
-- The cycle found for my input is 1735

-- Helper code for finding the repeating cycle
findVector :: [Int] -> [Int] -> [Int]
findVector v l = findVector' v l 0
    where 
        findVector' _ [] _  = []
        findVector' v l  c  | v == tv   = [c] ++ nf   
                            | otherwise =        nf
            where
                tv = take (length v) l
                nf = findVector' v (tail l) (c+1)

-- Calculations for part 2
calculatePart2 :: Distances -> Distance
calculatePart2 hs = dCycle * factor + remainingStartAndEnd
    where
        deltaHs = deltaList hs
        vector  = take (div fallingPart1 2) $ drop fallingPart1 deltaHs
        cycle   = head $ deltaList $ findVector vector deltaHs
        start   = cycle + length rocks
        hStart  = hs !! start
        hCycle2 = hs !! (start + cycle)
        dCycle  = hCycle2 - hStart
        (factor,remaining)   = divMod (fallingPart2 - start) cycle
        remainingStartAndEnd = hs !! (start + remaining)
        -- The delta's in a list
        deltaList tl = [ s-f | (f,s) <- zip tl (tail tl)] 


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 17  (Haskell)"
            pushDirections <- concat <$> repeat <$> readFile filename
            putStr "The height of the tower for part 1 is: "
            -- Even compiled the code is a bit slow (as in several seconds)
            putStrLn "         3065 *"
            let ths = towerHeights pushDirections
            -- print $ ths !! fallingPart1
            putStr "The height of the tower for part 2 is: "
            putStrLn "1562536022966 *"
            let heightPart2 = calculatePart2 ths
            -- print heightPart2
            putStrLn "\n0K.\n"

