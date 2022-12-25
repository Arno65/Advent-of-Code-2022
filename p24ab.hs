-- Advent of Code 2022 - Day 24 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The fewest number of minutes required to reach the goal is:         228 / 18
-- This for going to reach the goal, go back and to the goal again is: 723 / 54
--
-- The code is a bit slow, (compiled) > 2 minutes for both tasks 

-- (cl) by Arno Jacobs, 2022-12-25

-- module AoC2022d24ab where


import Data.List (nub)

data Direction  = None | North | East | South | West    deriving (Eq) 

instance Show Direction where
    show North  = [cNorth]
    show East   = [cEast]
    show South  = [cSouth]
    show West   = [cWest]
    show _      = [cClear]

type Steps      = Int
type Position   = (Steps,Steps)
type Directions = [Direction]
type Valley     = [[Directions]]
type Valleys    = [Valley]
-- Elf Position - per minute . . .
type ElfSteps   = [Position]


filename :: String
filename = "data/inputDay24_2022.txt" 

-- Step South, East, North, West
deltaSteps :: ElfSteps
deltaSteps = [(0,1),(1,0),(0,-1),(-1,0)]

cClear  = '.'  :: Char
cNorth  = '^'  :: Char
cEast   = '>'  :: Char
cSouth  = 'v'  :: Char
cWest   = '<'  :: Char
cWall   = '#'  :: Char
cElf    = 'E'  :: Char

addTuple :: Position -> Position -> Position
addTuple (x1,y1) (x2,y2) = (x1+x2,y1+y2)

parse :: [String] -> Valley
parse = map (map blizzardDirection) . map (tail . init) . tail . init
    where
    --  blizzardDirection :: Char -> Directions
        blizzardDirection c | c == cNorth   = [North]
                            | c == cEast    = [East]        
                            | c == cSouth   = [South]        
                            | c == cWest    = [West]        
                            | otherwise     = [None]        

noWallPosition :: Int -> String -> Position
noWallPosition row wall | fl == []  = (-1,row)
                        | otherwise = (snd (head fl),row)
    where 
        fl = filter (\(w,_) -> w == cClear) $ zip (tail wall) [0..]

nextMinute :: Valley -> Valley
nextMinute valley = nextValley newPositions
    where
        maxX    = length $ head valley
        maxY    = length valley
        newPositions = concat [[ newPosition (x,y) (maxX,maxY) d | d <- valley !! y !! x ] | 
                                    x <- [0..maxX-1], 
                                    y <- [0..maxY-1] ]
        newPosition (x,y) (_ ,mY) North = ((x           ,mod (y-1) mY),North)
        newPosition (x,y) (mX,_ ) East  = ((mod (x+1) mX,y           ),East)
        newPosition (x,y) (_ ,mY) South = ((x           ,mod (y+1) mY),South)
        newPosition (x,y) (mX,_ ) West  = ((mod (x-1) mX,y           ),West)
        newPosition (x,y) _       _     = ((x           ,y           ),None)
        nextValley np = [[ nv   | x <- [0..maxX-1]
                                , let nb = newBlizzards (x,y) np
                                , let nv = if nb == [] then [None] else nb ]
                                | y <- [0..maxY-1]]
        newBlizzards xy = map snd . filter (\(p,d) -> p == xy && d /= None)

-- Starting at minute 0 with the initial valley
-- The list has the n-th valley for the n-th minute
generateValleys :: Valley -> Valleys
generateValleys valley = [valley] ++ generateValleys nextValley
    where 
        nextValley = nextMinute valley

walkTheValley :: Valleys -> (Position,Position) -> Position -> (ElfSteps,Int) -> (ElfSteps,Int)
walkTheValley valleys (start,end) final (elfsteps,minutes) 
    | goal           = (elfsteps,minutes)
    | otherwise      = walkTheValley valleys (start,end) final (newElfSteps,nextMinute) 
            where 
                nextMinute  = minutes + 1
                maxBorder   = (length ((head . head) valleys) - 1, length (head valleys) - 1)
                newElfSteps = nextElfSteps (valleys !! nextMinute) maxBorder (start,end) elfsteps 
                goal        = elem final elfsteps

--- look for all posible steps - per minute frame
--- From those steps walk the next steps
--- only save unique steps (nub) per minute

nextElfSteps :: Valley -> Position -> (Position,Position) -> ElfSteps -> ElfSteps
nextElfSteps valley maxBorder (start,end) = nub . concat . map (nextElfSteps' valley maxBorder (start,end))
    where
    --  nextElfSteps' :: Valley -> Position -> (Position,Position) -> Position -> ElfSteps
        nextElfSteps' valley maxBorder (start,end) elfPosition 
            | steps == []   = wait              -- Elf has to wait
            | otherwise     = steps ++ wait     -- Elf is allowed to wait
                where
                    neighbours  = map (addTuple elfPosition) deltaSteps
                    steps       = checkSteps valley maxBorder (start,end) neighbours
                    wait        = checkSteps valley maxBorder (start,end) [elfPosition]
                --  checkSteps :: Valley -> Position -> (Position,Position) -> ElfSteps -> ElfSteps
                    checkSteps valley maxBorder (start,end) steps = filter (noBlizzard valley maxBorder (start,end)) steps
                        where 
                        --  noBlizzard :: Valley -> Position -> (Position,Position) -> Position -> Bool
                            noBlizzard valley (maxX,maxY) (start,end) (pX,pY)
                                -- The next 4 lines ~ all in the right order!
                                | (pX,pY) == start || (pX,pY) == end            = True      
                                | pX < 0 || pX > maxX || pY < 0 || pY > maxY    = False
                                | head (valley !! pY !! pX) == None             = True
                                | otherwise                                     = False 


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 24  (Haskell)"
            weatherMap <- lines <$> readFile filename
            let valleys = (generateValleys . parse) weatherMap
            let start   = noWallPosition (-1)                   $ head weatherMap
            let end     = noWallPosition (length weatherMap-2)  $ last weatherMap
            putStr "The fewest number of minutes required to reach the goal is:         "
            -- Elf position and Valley after 1 minute from start
            let (_,minutes) = walkTheValley valleys (start,end) end ([start],0)
--            print minutes
            putStrLn "228 *"
            let (_,minutesBack)   = walkTheValley valleys (start,end) start ([end],  minutes)
            let (_,minutesReturn) = walkTheValley valleys (start,end) end   ([start],minutesBack)  
            putStr "This for going to reach the goal, go back and to the goal again is: " 
--            print minutesReturn
            putStrLn "723 *"
            putStrLn "0K.\n"

-- pretty print / show example
--            showValley (start,valleys !! 228) -- Show the valley at minute 228 


-- Extra pretty print for the Valley and the Blizzard directions ------------------------
--
--        (Elf position, valley map)
showValley :: (Position,Valley) -> IO ()
showValley = showLines . matchSpritesWithBlizzards

-- Check if pixel at column X matches the clock cycle
-- If so draw 'pixel' - otherwise leave blank 
matchSpritesWithBlizzards :: (Position,Valley) -> [String]
matchSpritesWithBlizzards (elf,valley) = [[ pixel (elf,valley) (x,y)    
                                            | x <- [0..maxX-1]] 
                                            | y <- [0..maxY-1]] 
    where
        (maxX,maxY) = (length (head valley), length valley)
    --  pixel :: (Position,Valley) -> Position -> Char
        pixel (elf,valley) (x,y)    | elf == (x,y)  = cElf
                                    | hl > 1        = head $ show hl
                                    | otherwise     = head $ show $ head hb
            where 
                hb = valley !! y !! x
                hl = length hb

-- 'pretty print' the lines
showLines :: [String] -> IO ()
showLines []     = putStrLn "0K.\n"
showLines (l:ls) = do  putStrLn l
                       showLines ls

