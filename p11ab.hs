-- Advent of Code 2022 - Day 11 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The level of monkey business after    20 rounds is:      151312
-- The level of monkey business after 10000 rounds is: 51382025916
-- 
-- (cl) by Arno Jacobs, 2022-12-28

-- 
module AoC2022d11ab where

import Data.Char (isNumber)
import Data.List (sort)


data Part       = Part1 | Part2             deriving (Eq,Show)
data Operator   = Plus | Times | Square     deriving (Eq)

instance Show Operator where
    show Plus   = "+"
    show Times  = "x"
    show Square = "^"

type Index      = Int
type Item       = Int
type Items      = [Item] 
type Number     = Int
type Numbers    = [Number]
type Operation  = (Operator,Number)

data Monkey     = Monkey    {   number      :: Number
                            ,   starting    :: Items
                            ,   operation   :: Operation
                            ,   divisible   :: Number
                            ,   throw       :: (Index,Index)
                            ,   inspected   :: Number
                            }   deriving (Eq,Show)

type Monkeys    = [Monkey]


filename :: String
filename = "data/inputDay11_2022.txt"

roundsPart1, roundsPart2 :: Number
roundsPart1 =    20
roundsPart2 = 10000

-- getters and setters
--
getMonkeyNumber :: Monkey -> Number
getMonkeyNumber (Monkey number _ _ _ _ _ ) = number
getStarting :: Monkey -> Items
getStarting (Monkey _ starting _ _ _ _ )   = starting
getOperation :: Monkey -> Operation
getOperation (Monkey _ _ operation _ _ _ ) = operation
getDivisible :: Monkey -> Number
getDivisible (Monkey _ _ _ divisible _ _ ) = divisible
getThrow :: Monkey -> (Index,Index)
getThrow (Monkey _ _ _ _ throw _ )         = throw
getInspected :: Monkey -> Number
getInspected (Monkey _ _ _ _ _ inspected)  = inspected

incInspected :: Monkey -> Monkey
incInspected (Monkey { number, starting, operation, divisible, throw, inspected = ii }) =
              Monkey { number, starting, operation, divisible, throw, inspected = ii + 1 }

addStarting :: Item -> Monkey -> Monkey
addStarting i (Monkey { number, starting =  sl, operation, divisible, throw, inspected }) =
               Monkey { number, starting = nsl, operation, divisible, throw, inspected }
    where nsl = sl ++ [i]   -- Add new Item at end of the existing list

takeStarting :: Monkey -> (Items,Monkey) 
takeStarting (Monkey { number, starting =  sl, operation, divisible, throw, inspected }) =
          (il,Monkey { number, starting = nsl, operation, divisible, throw, inspected })
    where
        nsl = drop 1 sl     -- 'tail' will ERROR on an empty list
        il  = take 1 sl     -- 'head' will ERROR on an empty list
        
-- Only read the first (positive) Int in a String. NO Int will return 0
getNumber :: String -> Number
getNumber s     | hs == ""  = 0
                | otherwise = read hs 
    where hs = (takeWhile isNumber . dropWhile (not . isNumber)) s

getIndex :: String -> Index
getIndex = getNumber

-- Get list of (positive) Ints from an arbitrary String (safe)
getItems :: String -> Items
getItems ""     = []
getItems (c:cs) | isNumber c    = [i] ++ getItems (dropWhile isNumber cs)
                | otherwise     =        getItems (dropWhile (not . isNumber) cs)
                    where i = read (takeWhile isNumber (c:cs))

parse :: [String] -> Monkeys
parse []    =  []
parse ol    | mn == []  = parse $ tail ol
            | otherwise = [ Monkey  {   number      = head mn
                                    ,   starting    = sil
                                    ,   operation   = opp
                                    ,   divisible   = di 
                                    ,   throw       = tp
                                    ,   inspected   = 0 } ] ++ parse (drop 6 ol)
    where
        mn  = getItems (ol !! 0) -- Use this (also) to check for start of data set
        sil = getItems (ol !! 1)
        opp = readOperation (ol !! 2)
        di  = getNumber (ol !! 3)
        tp  = (getIndex (ol !! 4), getIndex (ol !! 5))
        readOperation s | elem '+' s            = (Plus,i)
                        | elem '*' s && i == 0  = (Square,2)
                        | otherwise             = (Times,i)
            where i = getNumber s

-- do one throw 
oneThrow :: Part -> Index -> Number -> Monkeys -> Monkeys
oneThrow part mn modAll monkeys    
    | wl == []                      = monkeys
    | otherwise                     = updateMonkeys mn t2mn nxtWL nxtm monkeys 
        where
            monkey      = monkeys !! mn      -- no range checking
            (wl,nxtm)   = takeStarting (incInspected monkey)
            hWL         = operate (head wl) (getOperation monkey)
            nxtWL       = if part == Part1 then div hWL 3 else mod hWL modAll
            isDivisible = (mod nxtWL (getDivisible monkey)) == 0
            t2mn        = (if isDivisible then fst else snd) $ getThrow monkey
        --  operate :: Number -> Operation -> Number
            operate wl (Plus,n)     = wl + n
            operate wl (Times,n)    = wl * n
            operate wl (Square,_)   = wl * wl     
        -- Update monkeys-list
        --  updateMonkeys :: Index -> Index -> Item -> Monkey -> Monkeys -> Monkeys
            updateMonkeys mn t2mn nxtWL nxtm monkeys = newMonkeys
                where
                    monkeys'    = take mn   monkeys  ++ [nxtm]     ++ drop (mn+1)   monkeys
                    t2monkey    = addStarting nxtWL (monkeys' !! t2mn)
                    newMonkeys  = take t2mn monkeys' ++ [t2monkey] ++ drop (t2mn+1) monkeys'

-- work one round - so let all monkeys throw all their items
oneRound :: Part -> Number -> Monkeys -> Monkeys
oneRound part modAll = oneRound' 0 part modAll
    where
        oneRound' mnrs part modAll monkeys
            | mnrs >= length monkeys    = monkeys 
            | otherwise                 = 
                oneRound' (mnrs+1) part modAll $ oneMonkey part mnrs modAll monkeys
                where 
                    -- do all throws for one monkey
                    --  oneMonkey :: Part -> Index -> Number -> Monkeys -> Monkeys
                    oneMonkey part mn modAll monkeys    
                        | monkeys == newMonkeys = monkeys
                        | otherwise             = oneMonkey part mn modAll newMonkeys 
                            where newMonkeys = oneThrow part mn modAll monkeys
            
-- work n-rounds                
nRounds :: Part -> Index -> Number -> Monkeys -> Monkeys
nRounds _    0 modAll monkeys = monkeys
nRounds part r modAll monkeys = nRounds part (r-1) modAll $ oneRound part modAll monkeys


monkeyBusiness :: Part -> Monkeys -> Number
monkeyBusiness part monkeys = max2Product $ nRounds part rounds modAll monkeys 
    where
        modAll  = product $ map getDivisible monkeys
        rounds  = if part == Part1 then roundsPart1 else roundsPart2
    -- pick the two heighest numbers from a list and multiply the two numbers
    --  max2Product :: Monkeys -> Number
        max2Product monkeys = (mxl !! 0) * (mxl !! 1)
            where mxl = reverse $ sort $ map getInspected monkeys

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 11  (Haskell)"
            allMonkeys <- parse <$> lines <$> readFile filename
            putStr "The level of monkey business after    "
            putStr $ show roundsPart1
            putStr " rounds is:      "
            print $ monkeyBusiness Part1 allMonkeys
            putStr "The level of monkey business after "
            putStr $ show roundsPart2
            putStr " rounds is: " 
            print $ monkeyBusiness Part2 allMonkeys
            putStrLn "\n0K.\n"

