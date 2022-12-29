-- Advent of Code 2022 - Day 22 task A only
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The password for part 1 is:  66292
--
-- Only a neat solution for the first part 
-- (My solution for part two doesn't have a generic solution.)
--
--
-- (cl) by Arno Jacobs, 2022-12-22

-- 
module AoC2022d22ab where

import Data.Char    (isNumber)

cTile = '.' :: Char
cWall = '#' :: Char
cVoid = ' ' :: Char

data Object     = Tile | Wall | Void                    deriving (Eq) 
data Rotation   = Clockwise | CounterClockwise | None   deriving (Eq)
data Direction  = North | East | South | West           deriving (Eq) 

instance Show Object where
    show Tile   = [cTile]
    show Wall   = [cWall]
    show Void   = [cVoid]

instance Show Rotation where
    show Clockwise          = "↻"
    show CounterClockwise   = "↺"  
    show None               = "."  

instance Show Direction where
    show North  = "^"
    show East   = ">"
    show South  = "v"
    show West   = "<" 

type Steps      = Int
type Position   = (Steps,Steps)
type Location   = (Position,Object)
type Map        = [Location]
type Cube       = [Map]
type Move       = (Steps,Rotation)
type Moves      = [Move]
type POV        = (Position,Direction)


filename :: String
filename = "data/inputDay22_2022.txt" 


rotate :: Direction -> Rotation -> Direction
rotate North Clockwise          = East
rotate North CounterClockwise   = West
rotate East  Clockwise          = South
rotate East  CounterClockwise   = North
rotate South Clockwise          = West
rotate South CounterClockwise   = East
rotate West  Clockwise          = North
rotate West  CounterClockwise   = South
rotate d     _                  = d

stepDirection :: Direction -> Position
stepDirection North  = ( 0,-1)
stepDirection East   = ( 1, 0)
stepDirection South  = ( 0, 1)
stepDirection West   = (-1, 0)

tuplesAdd :: Num a => (a,a) -> (a,a) -> (a,a)
tuplesAdd (f1,s1) (f2,s2) = (f1+f2,s1+s2) 

getObject :: Map -> Position -> Object
getObject map pxy   | mml == [] = Void
                    | otherwise = snd $ head mml
    where mml = filter (\(mp,_) -> mp == pxy ) map

getMap :: [String] -> Map
getMap = getMap' . init . init 

getMap' :: [String] -> Map
getMap' ls = removeVoids $ concat $ map (\(l,y) -> parseLine y l) $ zip ls [1..]
    where
        removeVoids = filter (\(_,obj) -> obj /= Void)             

parseLine :: Steps -> String -> Map
parseLine y l = map (\(p,x) -> parsePoint (x,y) p) $ zip l [1..]

parsePoint :: Position -> Char -> Location
parsePoint xy p | p == cTile    = (xy,Tile)
                | p == cWall    = (xy,Wall)
                | otherwise     = (xy,Void)

getPath :: [String] -> Moves
getPath [] = []
getPath ls = getPath' pathInput
    where
        pathInput = last ls
    --  getPath' :: String -> Moves
        getPath' [] = []
        getPath' p  | turn == []        = [(steps,None)]
                    | head turn == 'R'  = [(steps,Clockwise)]        ++ getPath' (tail turn)
                    | otherwise         = [(steps,CounterClockwise)] ++ getPath' (tail turn)
            where
                steps = read $ takeWhile isNumber p
                turn  = dropWhile isNumber p

-- Part1 with lots of code
--
getPassword :: Map -> Moves -> Int
-- getPassword []  _       = 0
getPassword map path    = 1000 * ep1Y + 4 * ep1X + directionValue dir1 
    where
        startPosition   = (fst . head) map
        ((ep1X,ep1Y),dir1) = walk2DPath map  path startPosition East
        directionValue East  = 0
        directionValue South = 1
        directionValue West  = 2
        directionValue North = 3

walk2DPath :: Map -> Moves -> Position -> Direction -> POV
walk2DPath map []                       cpXY dir  = (cpXY,dir)
walk2DPath map ((steps,rotation):rpath) cpXY dir  =
    walk2DPath map rpath newPosXY newDirection
        where
            newPosXY     = walkSteps map cpXY steps $ stepDirection dir
            newDirection = rotate dir rotation

walkSteps :: Map -> Position -> Steps -> Position -> Position
walkSteps _   pXY 0     _   = pXY
walkSteps map pXY steps dXY 
    | no  == Tile   = walkSteps map nXY (steps-1) dXY   -- Tile
    | no  == Wall   = pXY                               -- Wall
    | jXY == pXY    = pXY                               -- can't jump Void
    | otherwise     = walkSteps map jXY (steps-1) dXY   -- Void & jump
        where
            nXY = tuplesAdd pXY dXY
            no  = getObject map nXY
            jXY = jumpVoid map pXY dXY

jumpVoid :: Map -> Position -> Position -> Position
jumpVoid map (px,py) ( 0,-1) = jumpVoid' map (px,py) (  px,maxY) ( 0,-1)
    where maxY = getMaxY px map
jumpVoid map (px,py) ( 1, 0) = jumpVoid' map (px,py) (minX,  py) ( 1, 0)
    where minX = getMinX py map
jumpVoid map (px,py) ( 0, 1) = jumpVoid' map (px,py) (  px,minY) ( 0, 1)
    where minY = getMinY px map 
jumpVoid map (px,py) (-1, 0) = jumpVoid' map (px,py) (maxX,  py) (-1, 0)
    where maxX = getMaxX py map

jumpVoid' :: Map -> Position -> Position -> Position -> Position
jumpVoid' map oXY sXY dXY   | no == Tile    = sXY 
                            | otherwise     = oXY -- otherwise is Wall
    where no = getObject map sXY

getMinX, getMaxX, getMinY, getMaxY :: Steps -> Map ->  Steps
getMinX row    = minimum . map fst . map fst . filter (\((_,y),_) -> y == row)
getMaxX row    = maximum . map fst . map fst . filter (\((_,y),_) -> y == row)
getMinY column = minimum . map snd . map fst . filter (\((x,_),_) -> x == column)
getMaxY column = maximum . map snd . map fst . filter (\((x,_),_) -> x == column)


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 22  (Haskell) First part only"
            mapAndPath <- lines <$> readFile filename
            let map = getMap mapAndPath
            let path = getPath mapAndPath
            putStr   "The password for part 1 is: "
            print $ getPassword map path
            putStrLn "0K.\n"

