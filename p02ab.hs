-- Advent of Code 2022 - Day 2 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The total score for strategy 1:  9759
-- The total score for strategy 2: 12429
--
-- (cl) by Arno Jacobs, 2022-12-02

module AoC2022d02ab where

filename :: String
filename = "data/inputDay02_2022.txt"

-- Standard scores for Strategy 1 and 2
-- ["A X", "A Y", "A Z", "B X", "B Y", "B Z", "C X", "C Y", "C Z" ]
-- [ 4,     8,     3,     1,     5,     9,     7,     2,     6    ]
-- [ 3,     4,     8,     1,     5,     9,     2,     6,     7    ]

allDraws = ["A X","A Y","A Z","B X","B Y","B Z","C X","C Y","C Z"] 
pointsPart1 = [4,8,3,1,5,9,7,2,6]
pointsPart2 = [3,4,8,1,5,9,2,6,7]

-- NO safe head - but 0K. for here and now.
getPoints :: [(String,Int)] -> String -> Int
getPoints pl ts = (snd . head) $ filter (\(s,_) -> s == ts) pl

scorePart :: [Int] -> [String] -> Int
scorePart pp = sum . map (getPoints (zip allDraws pp)) 

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 2"
            day2 <- lines <$> readFile filename
            putStr   "The total score for strategy 1:  "
            print $ scorePart pointsPart1 day2
            putStr   "The total score for strategy 2: "
            print $ scorePart pointsPart2 day2 
            putStrLn "0K.\n"


