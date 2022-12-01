-- Advent of Code 2022 - De INFI queeste - met code in 'vanilla' Haskell
-- https://aoc.infi.nl
--
-- Deel 1:
-- Gegeven de navigatie-instructies, vind de Manhattan afstand 
-- tussen het begin- en eindpunt. Deze is: 4
--
-- Deel 2:
-- Het woord wat de kerstman met zijn stappen in de sneeuw heeft
-- achtergelaten is : kersttrui
-- 
--
-- (cl) Arno Jacobs, 01-12-2022

module Infi2022 where


bestandsnaam :: String
bestandsnaam = "idata2022.txt"

type Positie    = (Int,Int)   
type Richting   = (Int,Int)
type Instructie = (String,Int)

-- Geordende lijst van een draaiing met stappen van 45 graden
-- kompas:  N - NO - O - ZO - Z  - ZW - W  - NW
-- graden:  0   45   90  135  180  225  270  315  
richtingen :: [Richting]
richtingen = [(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1)]

startRichting :: Richting
startRichting = head richtingen

startPunt :: Positie
startPunt = (0,0)

-- Splits instructie en grootheid
splitsData :: [String] -> [Instructie]
splitsData xs = map splitsRegel xs
    where
        splitsRegel l =   ( takeWhile (/=' ') l, 
                            naarInt (tail $ dropWhile (/=' ') l))
        -- Veilige conversie van string naar (ook negative) integers
        naarInt ('-':xs)    = - read xs
        naarInt xs          =   read xs

-- wandel o.b.v. instructies en sla alle stappen op voor taak 2
deWandeling :: Positie -> Richting -> [Instructie] -> [Positie]
deWandeling hp _  []            = [hp]
deWandeling hp hr ((si,ii):il)  
    | si == "loop" = markeerStappen hp np ++ deWandeling np nr il
    | otherwise    =                         deWandeling np nr il
        where
            (np,nr) = volgDeInstructie hp hr si ii  

-- instructie o.b.v. patroon 
volgDeInstructie :: Positie -> Richting -> String -> Int -> (Positie,Richting)
volgDeInstructie hp hr "spring" stappen = (zetStappen hp hr stappen, hr)
volgDeInstructie hp hr "loop"   stappen = (zetStappen hp hr stappen, hr)
volgDeInstructie hp hr _        draaing = (hp, veranderRichting hr draaing)

-- Zet de juiste stappen bij "loop" en "spring"
zetStappen :: Positie -> Richting -> Int -> Positie
zetStappen (px,py) (dx,dy) stappen = (px + stappen * dx, py + stappen * dy)

-- Draai naar de juiste richting bij "draai"
-- De 'mod' functie zet alle draaingen om naar de (index) draaiingen 
-- van 0 tot 360 graden
veranderRichting :: Richting -> Int -> Richting
veranderRichting hr graden = 
    richtingen !! (mod (hrix + draaiIndex) (length richtingen))
        where
            draaiIndex  = div graden 45
            hrix        = lijstIndex richtingen hr

-- Een veilige 'head' maar niet veilig voor 'veranderRichting'
-- Prima voor hier, nu en later
lijstIndex :: Eq a => [a] -> a -> Int
lijstIndex xs q = head $ [ ix | (e,ix) <- zip xs [0..], e == q ] ++ [-1]

-- Tja, functies in Haskell beginnen met 'lower case' letters, i.p.v. Manhattan...
manhattanAfstand :: Positie -> Int
manhattanAfstand (x,y) = x+y


-- Deel 2 

-- Markeer alle stappen die de Kerstman heeft gezet
markeerStappen :: Positie -> Positie -> [Positie]
markeerStappen (spx,spy) (epx,epy) = 
    stapVoorStap (spx,spy) (signum dx,signum dy) stappen
        where
            dx      = epx - spx
            dy      = epy - spy
            stappen = 1 + max (abs dx) (abs dy)

stapVoorStap :: Positie -> Richting -> Int -> [Positie]
stapVoorStap  _       _      0          = []
stapVoorStap (px,py) (dx,dy) stappen    = 
    [(px,py)] ++ stapVoorStap (px+dx,py+dy) (dx,dy) (stappen-1)

tekenPosities :: [Positie] -> String
tekenPosities p = concat $ reverse 
                    [[ isBetreden (x,y) p  | x <- [0..mx]] ++ "\n" | y <- [0..my]]  
    where
    -- Bepaal de grenzen van het bewandelde terrein
    -- Minimum is op (0,0) gehouden. Negatieve posities bleken niet aanwezig. 
        mx = maximum $ map fst p
        my = maximum $ map snd p
        isBetreden p ps | elem p ps = '*'
                        | otherwise = ' '

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - De Infi queeste"
            instructies <- splitsData <$> lines <$> readFile bestandsnaam
            let pad = deWandeling startPunt startRichting instructies
            -- Deel 1
            putStr "\nDe Manhattan afstand tussen begin- en eindpunt is: "
            print $ manhattanAfstand $ last $ pad
            -- Deel 2
            putStrLn "\nDe door de Kerstman achtergelaten tekst is:\n"
            putStrLn $ tekenPosities pad
            putStrLn "0K.\n"

