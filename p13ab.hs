-- Advent of Code 2022 - Day 13 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of the indices of the right ordered pairs is:  5684
-- The decoder key for the distress signal is:           22932
--
-- (cl) by Arno Jacobs, 2022-12-13

-- --- -- -- --- -- -- --- -- -- --- -- -- --- -- -- --- -- -- --- -- -- --- -- 
-- Rewrite with use of "Ord instance" - this for comparing pairs of packets
-- I'm not happy with the functions: comparePair and comparePair'

module AoC2022d13ab where

import Data.Char (isNumber,isPunctuation)

type PairOfPackets = (String,String)

filename :: String
filename = "data/inputDay13_2022.txt"

cLBracket   = '['   :: Char
cRBracket   = ']'   :: Char
sComma      = ","   :: String
cComma      = head sComma

-- Adding packets to the list
add2 = "[[2]]"      :: String
add6 = "[[6]]"      :: String

-- Needed for getting a list of numbers, comma seperated
isComma, isLBracket :: Char -> Bool
isComma    c = c == cComma
isLBracket c = c == cLBracket

-- Every three (3) lines of input into a pair of String
-- (Skipping the empty third line)
makePairs :: [String] -> [PairOfPackets]
makePairs []        = []
makePairs (l:r:xs)  | xs == []  = [(l,r)]
                    | otherwise = [(l,r)] ++ makePairs (tail xs)

-- Get 'standard' list of positive (only) Ints from an arbitrary String (safe)
-- Setting the correct range of numbers is done in function: comparePair'
getInts :: String -> [Int]
getInts []      = []
getInts (c:cs)  
    | isNumber c    = [read (takeWhile isNumber (c:cs))] ++ nxt
    | otherwise     = getInts cs 
        where 
            nxt = getInts (dropWhile isPunctuation $ dropWhile isNumber (c:cs))

-- Compare one (1) pair of packet data
comparePair :: PairOfPackets -> Bool
comparePair ([],_)          = True
comparePair (_,[])          = False
comparePair (el:ls,er:rs)   
    |  el == er && isPunctuation el = comparePair (ls,rs)
    |  otherwise                    = comparePair' ((el:ls),(er:rs))
        where
            comparePair' :: PairOfPackets -> Bool
            comparePair' ([],_)         = True
            comparePair' (_,[])         = False
            comparePair' (el:ls,er:rs)  
                | er == cRBracket                   = False
                | el == cRBracket                   = True
                | isNumber el && isPunctuation er   = comparePair (nls,(er:rs)) 
                | isPunctuation el && isNumber er   = comparePair ((el:ls),nrs)  
                | hil == hir                        = comparePair (ls,rs)
                | compareInts hil hir               = True
                | compareInts hir hil               = False
                | otherwise                         = comparePair (ls,rs)
                    where
                        -- get numbers from list
                        hil = getInts $ takeWhile isNumberPart (el:ls)
                        hir = getInts $ takeWhile isNumberPart (er:rs)
                        isNumberPart c = isNumber c || isComma c || isLBracket c
                        -- comparing list of Ints
                        compareInts [] [] = True
                        compareInts [] _  = True
                        compareInts _ []  = False
                        compareInts (l:ls) (r:rs)   
                            | l > r     = False
                            | l < r     = True    
                            | otherwise = compareInts ls rs 
                        -- 're-bracket' first number
                        nls = rebracket (el:ls) 
                        nrs = rebracket (er:rs) 
                        rebracket s =   [cLBracket] ++ takeWhile isNumber s ++ 
                                        [cRBracket] ++ dropWhile isNumber s


-- Part 1
rightOrderedPairsIndicesSum :: [PairOfPackets] -> Int
rightOrderedPairsIndicesSum pl = 
    sum $ map snd $ filter (\(t,_) -> t) $ zip (map comparePair pl) [1..]

-- Part 2
decoderKey :: [PairOfPackets] -> Int
decoderKey pps = ix2 * ix6
    where 
        -- The list of pairs to a list AND adding [[2]] and [[6]] to that list
        -- And sort the list via function: comparePair
        spls    = packetSort $ pairsToList pps ++ [add2] ++ [add6]
        ix2     = indexPacket add2 spls
        ix6     = indexPacket add6 spls

-- Pairs to elements on a list
pairsToList :: [PairOfPackets] -> [String]
pairsToList = concat . map (\(l,r) -> [l] ++ [r])

-- Index starting from 1
indexPacket :: String -> [String] -> Int
indexPacket tp pl   | hl == []  = (-1)
                    | otherwise = head hl
    where 
        hl = map snd $ filter (\(p,_) -> p == tp) $  zip pl [1..]

-- quicksort algorithm with packet ordering
packetSort :: [String] -> [String]
packetSort []     = []
packetSort (p:ps) = packetSort smaller ++ equal ++ packetSort bigger
                where
                    equal   = [p] ++ filter (==p) ps
                    smaller = filter (\tp -> (not (orderedPackets p tp))) ps
                    bigger  = filter (\tp ->      (orderedPackets p tp))  ps
                    orderedPackets t1 t2 = comparePair (t1,t2)


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 13  (Haskell)"
            day13 <- makePairs <$> lines <$> readFile filename
            putStr "The sum of the indices of the right ordered pairs is:  "
            print $ rightOrderedPairsIndicesSum day13
            putStr "The decoder key for the distress signal is:           "
            print $ decoderKey day13            
            putStrLn "0K.\n"

