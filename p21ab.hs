-- Advent of Code 2022 - Day 21 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- A monkey called root yells:       38914458159166
-- The number for the equality test:  3665520865940

-- (cl) by Arno Jacobs, 2022-12-21

-- module AoC2022d21ab where

import Data.Char

filename :: String
filename = "data/inputDay21_2022.txt" 

root        =            "root" :: String
human       =            "humn" :: String
startMin    =             0     :: Int
startMax    =    99999999999999 :: Int 


data Operator = Plus | Minus | Times | DivideBy | Nop
                    deriving (Eq)

instance Show Operator where
    show Plus       = "+"
    show Minus      = "-"
    show Times      = "x"
    show DivideBy   = "÷"
    show Nop        = " "

type Number = Int

data Evaluate = I Number | E Evaluate Operator Evaluate 
            deriving (Eq)

instance Show Evaluate where
    show (I number)     = show number
    show (E l opr r )   = "(" ++ show l ++ show opr ++ show r ++ ")"

data Job = N Number | J Monkey Operator Monkey | None
            deriving (Eq,Show)

type Monkey = String
type Line = (Monkey,Job)
type Lines = [Line]


getOperation :: Char -> Operator
getOperation '+' = Plus
getOperation '-' = Minus
getOperation '*' = Times
getOperation '/' = DivideBy
getOperation _   = Nop

-- Convert the input to a list of tuples of monkeys and jobs
parse :: String -> Line
parse line  | hasNumber = (monkey,N number)
            | otherwise = (monkey,J monkeyLeft operation monkeyRight)
    where
        monkey      = takeWhile (/= ':') line
        job         = dropAndTail line
        hasNumber   = isNumber $ head job
        number      = read job
        monkeyLeft  = takeWhile (/= ' ') job
        operation   = getOperation $ (head . dropAndTail) job
        monkeyRight = (dropAndTail. dropAndTail) job
        dropAndTail = tail . dropWhile (/= ' ')

setRootMinus :: Lines -> Lines
setRootMinus jobs = [(root, J ml Minus mr)] ++ 
                    filter (\(tm,_) -> tm /= root) jobs
    where 
        (J ml _ mr) = snd $ head $ filter (\(tm,_) -> tm == root) jobs

setHuman :: Lines -> Number -> Lines
setHuman jobs n =   [(human,N n)] ++ 
                    filter (\(tm,_) -> tm /= human) jobs

getJob :: Monkey -> Lines -> Job
getJob monkey jobs  | lh == []  = None 
                    | otherwise = (snd . head) lh 
    where 
        lh = filter (\(testMonkey,_) -> testMonkey == monkey) jobs

isNum :: Job -> Bool
isNum (N _) = True
isNum _     = False

buildTree :: Monkey -> Lines -> Evaluate
buildTree monkey jobs
    | isNum monkeyJob   = I number
    | otherwise         = E evalLeft operator evalRight
        where
            monkeyJob   = getJob monkey jobs
            (N number)  = monkeyJob
            (J monkeyLeft operator monkeyRight) = monkeyJob
            evalLeft    = buildTree monkeyLeft  jobs
            evalRight   = buildTree monkeyRight jobs
            
evaluate :: Bool -> Evaluate -> Maybe Number
evaluate _         (I number)       = Just number
evaluate strictDiv (E l operator r)   
    | er == 0                   = Nothing   -- division by zero
    | operator == Plus          = Just $ el   +   er
    | operator == Minus         = Just $ el   -   er
    | operator == Times         = Just $ el   *   er
    -- if operator is 'DiviseBy' and there is a remainer > 0
    -- This test is only done if 'strict' division is applied 
    | m /= 0 && strictDiv       = Nothing   -- not divisable ~ remainder > 0
    | otherwise                 = Just $ el `div` er
        where
            maybeLeft   = evaluate strictDiv l
            maybeRight  = evaluate strictDiv r
            (Just el)   = maybeLeft
            (Just er)   = maybeRight
            (d,m)       = divMod el er

-- Part 1 
-- A tree is build from the parsed input. That is evaluated.
evaluateLines :: Monkey -> Bool -> Lines -> Maybe Number
evaluateLines monkey strictDiv = evaluate strictDiv . buildTree monkey

-- Part 2
-- The 'root' operator is set to 'Minus'
-- Pick a low and high range for 'humn' and start the heuristics
-- Like a Higher-Lower games
-- For this the strict division in 'evaluate' is turned OFF
-- Only when the 'humn' value is found with NO-strict division
-- then 'findUp' will look for the correct 'humn' value 
-- by testing 'humn' values with strict division.
--
equalityTest :: Lines -> Monkey -> Monkey -> Number
equalityTest jobs monkey variableName = 
     findUp (setRootMinus jobs) monkey 
            ((higherLower jobs monkey variableName startMin startMax)-1)

higherLower :: Lines -> Monkey -> Monkey -> Number -> Number -> Number
higherLower jobs monkey variableName testLow testHigh
    | test == (Just 0)  = newHuman
    | test >  (Just 0)  = higherLower jobs monkey variableName (newHuman-1) (testHigh+1)
    | otherwise         = higherLower jobs monkey variableName (testLow-1) (newHuman+1)
        where
            newHuman    = testLow + div (testHigh - testLow) 2
            test        = evaluateLines monkey False $ setHuman jobs newHuman

findUp :: Lines -> Monkey -> Number -> Number
findUp jobs monkey ff   | test == (Just 0)  = ff
                        | otherwise         = findUp jobs monkey (ff+1)
    where
        test = evaluateLines monkey True $ setHuman jobs ff


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 21  (Haskell)"
            jobs <- map parse <$> lines <$> readFile filename
            putStr   "A monkey called root yells:       "
            let er = evaluateLines root True jobs
            let (Just yell) = er 
            putStrLn $ if (er == Nothing) then "None" else show yell 
            putStr   "The number for the equality test:  "
            print $ equalityTest (setRootMinus jobs) root human
            putStrLn "0K.\n"

 
