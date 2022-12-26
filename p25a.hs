-- Advent of Code 2022 - Day 25 task A
-- Solution in Haskell
-- (Ter leering ende vermaeck...)
--
-- The SNAFU number supplied to Bob's console is: 2-1-110-=01-1-0-0==2
--
-- (cl) by Arno Jacobs, 2022-12-26

module AoC2022d25a where
  
import Data.Char

filename :: String
filename = "data/inputDay25_2022.txt"

-- Convert SNAFU to 10-base integer
intSNAFU :: String -> Int
intSNAFU = foldl (\p d -> d + p*5) 0 . map cdSNAFU
    where
        cdSNAFU '-' = -1 
        cdSNAFU '=' = -2
        cdSNAFU  c  = ord c - 48

intToBase5list :: Int -> [Int]
intToBase5list 0 = []
intToBase5list i = intToBase5list d ++ [m]
    where (d,m) = divMod i 5

base5toSNAFU :: [Int] -> String
base5toSNAFU = base5toSNAFU' . reverse
    where
        base5toSNAFU' [] = []
        base5toSNAFU' (d1:[]) 
            | d1 < 3    = [chr (d1+48)]
            | d1 == 3   = "1="
            | d1 == 4   = "1-"
            | otherwise = "10"
        base5toSNAFU' (d1:d2:ds)    
            | d1 < 3    = base5toSNAFU' (d2:ds) ++ [chr (d1+48)]
            | d1 == 3   = base5toSNAFU' ((d2+1):ds) ++ "="
            | d1 == 4   = base5toSNAFU' ((d2+1):ds) ++ "-"
            | otherwise = base5toSNAFU' ((d2+1):ds) ++ "0"

base10toSNAFU :: Int -> String
base10toSNAFU = base5toSNAFU . intToBase5list 
    
    

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 25  (Haskell)"
            base10sum <- sum <$> map intSNAFU <$> lines <$> readFile filename
            putStr "The SNAFU number supplied to Bob's console is: "
            putStrLn $ base10toSNAFU base10sum
            putStrLn "\n0K.\n"
