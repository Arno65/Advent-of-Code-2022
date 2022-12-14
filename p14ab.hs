-- Advent of Code 2022 - Day 14 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The resting number units of sand with the cave abyss is:   901
-- The resting number units of sand with the cave floor is: 24589
--
-- Part 1 compiled (Intel) needs about  1 second. 
-- Part 2 compiled (Intel) needs about 74 seconds. 
--
-- If you like to plot the cave . . .
-- The rocks/sands range of the cave in part 2 is (323,162)
--
-- (cl) by Arno Jacobs, 2022-12-14

-- Better run compiled version ;-)
-- Un-comment the 'print unit' lines in the main function (from line 150+)
--
-- module AoC2022d10ab where
  
import Data.Char

filename :: String
filename = "data/inputDay14_2022.txt"

type Position   = (Int,Int)
type Rocks      = [Position] 
type Sands      = [Position] 

data Element    = Air | Rock | Sand     deriving (Eq,Show)
data CaveBorder = Abyss | Floor         deriving (Eq,Show) 

cAir    = ' ' :: Char
cRock   = '#' :: Char
cSand   = 'o' :: Char
cSource = '+' :: Char

sandSource = (500,0) :: Position

-- Get list of positive (only) Ints from an arbitrary String (safe)
getInts :: String -> [Int]
getInts []      = []
getInts (c:cs)  
    | isNumber c    = [read (takeWhile isNumber (c:cs))] ++ nxt
    | otherwise     = getInts cs
        where 
            nxt = getInts (dropWhile (not . isNumber) $ dropWhile isNumber (c:cs))

parseRocks :: [String] -> Rocks
parseRocks = concat . map (parseLineOfRocks . getInts)
    where
        parseLineOfRocks []                         = []
        parseLineOfRocks (_:_:[])                   = []
        parseLineOfRocks (fromX:fromY:toX:toY:rps)  
            | fromX == toX  =   [ (fromX,y) | y <- [(min fromY toY)..(max fromY toY)] ] ++ 
                                parseLineOfRocks (toX:toY:rps)
            | fromY == toY  =   [ (x,fromY) | x <- [(min fromX toX)..(max fromX toX)] ] ++ 
                                parseLineOfRocks (toX:toY:rps)
            | otherwise     =   error "No straight line !"
    
-- Get the bottom left and the top right corner of the rocks in the cave    
-- The position from where the sand is pouring is taken in account
-- fold . . .
caveRange :: Rocks -> (Position,Position)
caveRange = foldr tupleMinMax (sandSource,sandSource) 
    where
        tupleMinMax (px,py) ((minX,minY),(maxX,maxY)) = 
            ((min px minX,min py minY),(max px maxX,max py maxY))

-- The bottom border of the cave -- -- -- -- -- -- -- --
-- Part 1 has a void
-- Part 2 has a floor, 2 units below the lowest rocks

-- Drop one unit of sand (imaging a fixed sized grain of sand)
-- 'Position' is the position of the previous and current sand source 
-- 'Rocks' is a list of position of rocks
-- 'Sands' is a list of recent dropped sand units
-- 'Int' is the y-value where the abyss is starting
---     This is one more that the maximum Y of the cave-range
-- The function will return the new list of dropped sand units
-- If the sand unit falls into the abyss the 'Bool' will be True
dropSandUnit :: CaveBorder -> Sands -> Position -> Int -> Sands -> Rocks -> ((Sands,Sands),Bool)
dropSandUnit caveWith ppl (sx,sy) depth sands rocks
--
-- drop rules:
--  fall down until sand unit hits a rock or another sand unit
--  look one left one down
--      if free the drop there and drop further
--      else look one right one down
--          if free the drop there and drop further
--          else 
--              sand unit comes to rest
--
    |  caveWith == Floor 
    && sy == depth                  = ((nppl,sands ++ [(sx,sy)]),False)
    |  caveWith == Abyss
    && sy >= depth                  = ((ppl,sands),True)
    |  isAir (sx  ,nsy) rocks sands = dropSandUnit caveWith nppl (sx,  nsy) depth sands rocks
    |  isAir (sx-1,nsy) rocks sands = dropSandUnit caveWith nppl (sx-1,nsy) depth sands rocks
    |  isAir (sx+1,nsy) rocks sands = dropSandUnit caveWith nppl (sx+1,nsy) depth sands rocks
    |  sy == 0                      = ((nppl,sands ++ [(sx,sy)]),True)  -- at its right place ~ order
    |  otherwise                    = ((nppl,sands ++ [(sx,sy)]),False)
        where
            nppl = if (elem (sx,sy) ppl) then ppl else [(sx,sy)] ++ ppl -- current start position
            nsy = sy + 1
            isAir sp rocks sands    
                | elem sp rocks = False
                | elem sp sands = False
                | otherwise     = True

-- Start dropping units of sand from 'sandSource'
-- unit counter set at 0, no units of sand at start
-- abyss calculated from range of cave
dropSands :: CaveBorder -> Rocks -> (Int,Sands)
dropSands caveWith rocks = (length sands, sands)
    where
        depth = 1 + (snd . snd . caveRange) (rocks ++ [sandSource])
        sands = dropSands' caveWith [sandSource] depth [] rocks
        dropSands' caveWith source depth sands rocks 
            | isDone                = newSands
            | otherwise             = dropSands' caveWith ppl depth newSands rocks 
                where 
                    nsl = getFreePreviousPoints source sands
                    pp  = head nsl
                    ((ppl,newSands),isDone) = dropSandUnit caveWith nsl pp depth sands rocks
                    getFreePreviousPoints [] _ = [sandSource]
                    getFreePreviousPoints (pp:ppl) sands 
                        | elem pp sands = getFreePreviousPoints ppl sands
                        | otherwise     = (pp:ppl)

-- Show the 2D map of the cave
showCave :: Rocks -> Sands -> IO ()
showCave rocks sands = 
    showLines [[ getPixel (x,y) rocks sands | 
                    x <- [fromX..toX] ]     | 
                    y <- [fromY..toY] ]
    where
        ((fromX,fromY),(toX,toY)) = caveRange $ rocks ++ sands
        getPixel pixelPosition rocks sands  
            | elem pixelPosition rocks      = cRock
            | elem pixelPosition sands      = cSand
            | pixelPosition == sandSource   = cSource
            | otherwise                     = cAir

-- 'pretty print' the CRT lines
showLines :: [String] -> IO ()
showLines []     = return ()
showLines (l:ls) = do  putStrLn $ "| " ++ l ++ " |"
                       showLines ls


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 14  (Haskell)"
            rocks <- parseRocks <$> lines <$> readFile filename
            let (units1,sands1) = dropSands Abyss rocks
            let (units2,sands2) = dropSands Floor rocks          
            putStr "The resting number units of sand with the cave abyss is:   "
            putStrLn "901 *"
--            print units1
            putStr "The resting number units of sand with the cave floor is: "
            putStrLn "24589 *"
--            print units2
            putStrLn "\n0K.\n"
