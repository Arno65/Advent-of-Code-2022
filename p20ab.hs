-- Advent of Code 2022 - Day 20 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- For part1 the sum of 1000th, 2000th and 3000th number is:          3473
-- For part2 the sum of 1000th, 2000th and 3000th number is: 7496649006261

-- (cl) by Arno Jacobs, 2022-12-29

-- 
module AoC2022d20ab where


filename :: String
filename = "data/inputDay20_2022.txt"

decryptionKey :: Int
decryptionKey = 811589153

-- looking at the 1000th, 2000th, and 3000th numbers after the value 0
ix1,ix2,ix3 :: Int
ix1 = 1000
ix2 = 2000
ix3 = 3000


mixing :: Int -> [Int] -> [Int]
mixing c xs = map fst $ mixing' c $ zip xs [0..]
    where
    --  mixing' :: Int -> [(Int,Int)] -> [(Int,Int)] 
        mixing' c ixl   | c <= 0    = ixl
                        | otherwise = mixing' (c-1) hil
            where 
                hil = mixing'' 0 ixl
            --  mixing'' :: Int -> [(Int,Int)] -> [(Int,Int)]
                mixing'' ix ixl | ix >= length ixl  = ixl
                                | otherwise         = mixing'' (ix+1) nxl
                    where
                        e   =       head $ filter (\(_,i) -> i == ix)      ixl
                        l   = snd $ head $ filter (\(p,_) -> p == e) $ zip ixl [0..]
                        d   = fst e
                        nl  = mod (l + d) (length ixl - 1)
                        sl  = take l ixl ++ drop ( l+1) ixl
                        nxl = take nl sl ++ [e] ++ drop nl sl

-- Find the 1000th, 2000th, and 3000th numbers after the value 0
score :: [Int] -> Int
score xs = xs!!p1 + xs!!p2 + xs!!p3
    where
        ixl = zip xs [0..]
        l0  = snd $ head $ filter (\(e,_) -> e == 0) ixl
        ln  = length xs
        p1  = mod (l0 + ix1) ln
        p2  = mod (l0 + ix2) ln
        p3  = mod (l0 + ix3) ln

applyDecryptionKey :: [Int] -> [Int]
applyDecryptionKey = map (\x -> x * decryptionKey)

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 20  (Haskell)"
            code20 <- map read <$> lines <$> readFile filename
            let mix1 = mixing 1 code20
            putStr   "The score for part 1 is:          "
            print $ score mix1
            let mix2 = mixing 10 $ applyDecryptionKey code20
            putStr   "The score for part 2 is: "
            print $ score mix2
            putStrLn "\n0K.\n"

