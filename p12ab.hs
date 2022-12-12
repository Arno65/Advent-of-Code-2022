-- Advent of Code 2022 - Day 12 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The fewest steps from Start to End is:          456
-- The fewest steps from a lowest point to End is: 454
-- 
-- (cl) by Arno Jacobs, 2022-12-12

-- module AoC2022d12ab where

import Data.Char
import Data.List
  
type Position       = (Int,Int)
type Direction      = (Int,Int)
type Height         = Int
type HeightmapRow   = [Height]
type Heightmap      = [HeightmapRow]

data Steps =    Steps { position    :: Position, 
                        neighbours  :: [Position], 
                        steps       :: Int }
                    deriving (Eq,Ord,Show)

type Track = [Steps]

-- North - East - South - West
directions :: [Direction]
directions = [(0,1),(1,0),(-1,0),(0,-1)]


filename :: String
filename = "data/inputDay12_2022.txt"

-- The heightmap is (136,41)

-- By the rules
start = 'S' :: Char
end   = 'E' :: Char

-- given by rules
startHeight = readHeight 'a' :: Int
endHeight   = readHeight 'z' :: Int

-- helpers

-- Find (first) index of element 'e' in list 'l'
-- Return (-1) is element is NOT in list
index :: Eq a => a -> [a] -> Int
index e l = head $ [ix | (te,ix) <- zip l [0..], e==te] ++ [-1]

readHeight :: Char -> Int
readHeight c = ord c - ord 'a'

-- rework the input file of the map
-- Get start- and end-positions and get all heights
parseMap :: [String] -> ((Position,Position),Heightmap)
parseMap = parseMap' 0 (0,0) (0,0) []
    where
        parseMap' row sp ep hm []       = ((sp,ep),hm) 
        parseMap' row sp ep hm (mr:mrs) = parseMap' (row+1) nsp nep nhm mrs
            where
                nsp = if (elem start mr) then findPoint start (0,row) mr else sp                
                nep = if (elem end   mr) then findPoint end   (0,row) mr else ep
                nhm = hm ++ [parseMapLine mr]
                parseMapLine = map parsePoint
                parsePoint c | c == start = startHeight
                             | c == end   = endHeight
                             | otherwise  = readHeight c    -- height indication
                findPoint fc (_,py) hl = ((index fc hl),py)

createTrack :: [Position] -> Position -> Heightmap -> Int -> [Position] -> Track -> Track
createTrack cpl ep hm step plh ct
    | elem ep cpl   = ntrl     
    | cnl == []     = ntrl -- createTrack cnll ep hm (step+1) npll ntll     -- one step down ?
    | otherwise     = createTrack cnl  ep hm (step+1) nplh ntrl
        where
            cnl  = nub $ concat $ map (findNeighbours plh hm) cpl
            nplh = nub $ cpl ++ cnl
            ntrl = ct ++ [ Steps {  position = cp, 
                                    neighbours = nl, 
                                    steps = step } |
                            cp <- cpl, 
                            let nl = findNeighbours plh hm cp ]
  
-- simple adding - but here for a tuple
addStep :: Direction -> Position -> Position            
addStep (p1x,p1y) (p2x,p2y) = (p1x+p2x,p1y+p2y)

-- A bit of a safe function
-- Return (-1) if position is Out of Range OR an empty heightmap
heightValue :: Position -> Heightmap -> Int
heightValue _       []  = (-1)
heightValue (px,py) hm  | inRange   =  hm !! py !! px
                        | otherwise = (-1) 
    where
        hy = length hm
        hx = length $ head hm 
        inRange = px >= 0 && px < hx && py >= 0 && py < hy

findNeighbours :: [Position] -> Heightmap -> Position -> [Position]
findNeighbours plh hm cp =  
    [ np |  d <- directions, 
            let np = addStep d cp,
            not (elem np plh),           -- skip the paths we had
            let hv = heightValue np hm,
            (hcp == hv) || ((hcp+1) == hv) ]
        where
            hcp = heightValue cp hm

findLowerNeighbours :: [Position] -> Heightmap -> Position -> [Position]
findLowerNeighbours plh hm cp =  
    [ np |  d <- directions, 
            let np = addStep d cp,
            not (elem np plh),           -- skip the paths we had
            let hv = heightValue np hm - 1,
            (hcp >= hv) ]
        where
            hcp = heightValue cp hm

getPosition :: Steps -> Position
getPosition (Steps position _ _) = position

getNeighbours :: Steps -> [Position]
getNeighbours (Steps _ neighbours _) = neighbours

getSteps :: Steps -> Int
getSteps (Steps _ _ steps) = steps

noNeighbours :: Steps -> Bool
noNeighbours s = getNeighbours s == []

getMinimalSteps :: [Steps] -> [Steps]
getMinimalSteps []      = []
getMinimalSteps (se:sl) = [(Steps p n minsl)] ++ getMinimalSteps neqpl
    where
        (Steps p n s) = se
        eqpl    = [se] ++   filter (\tp -> getPosition tp == p) sl
        neqpl   =           filter (\tp -> getPosition tp /= p) sl
        minsl   = minimum $ map getSteps eqpl


getEndPoint :: Position -> Track -> [Int]
getEndPoint ep []       = []
getEndPoint ep (p:ps)   | cp == ep  = getEndPoint ep ps ++ [steps]
                        | otherwise = getEndPoint ep ps 
    where
        cp    = getPosition p
        steps = getSteps p

minimalSteps :: Position -> Track -> Int
minimalSteps ep ps  | epl == [] = (-1)
                    | otherwise = minimum epl
    where
        epl = getEndPoint ep ps

-- Part 1
-- stepsToEndPoint ((start point, end point), heighmap) -> minimal number of steps
stepsToEndPoint1 :: ((Position,Position),Heightmap) -> Int 
stepsToEndPoint1 = stepsToEndPoint1' 0
    where
        stepsToEndPoint1' steps ((sp,ep),hm)    | mSteps > 0    = steps + mSteps
                                                | hl == []      = -1    -- Only working for ONE time 'step down'
                                                | otherwise     = head hl
            where
                tracks  = revCT [sp] 
                mSteps  = minimalSteps ep tracks 
                fms = getMinimalSteps $ filter noNeighbours tracks
                mps = map getPosition fms
                hsl = map getSteps fms
                hl  = take 1 [ s+1+msn | 
                                (p,s) <- zip mps hsl,
                                let hpl = findLowerNeighbours [] hm p, 
                                hpl /= [],
                                let msn = minimalSteps ep $ revCT hpl,
                                msn > 0 ]
                revCT pl = reverse $ createTrack pl ep hm 0 [] []
                
       
-- Part 2
-- Set start points at { (0,0)..(0,40) }
-- Calculate all minimal steps
-- Pick from all those tracks the minimum
--
stepsToEndPoint2 :: ((Position,Position),Heightmap) -> Int 
stepsToEndPoint2 (((spx,spy),ep),hm) = 
    minimum [ stepsToEndPoint1 (((spx,y),ep),hm) | y <- [0..length hm-1]]

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 12  (Haskell)"
            day12 <- parseMap <$> lines <$> readFile filename
            putStr "The fewest steps from Start to End is:          "
            print $ stepsToEndPoint1 day12
            putStr "The fewest steps from a lowest point to End is: "
            print $ stepsToEndPoint2 day12        -- slow in the runAll
            putStrLn "\n0K.\n"
            
