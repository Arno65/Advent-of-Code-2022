-- Advent of Code - day 7
-- I solved it with a files list
-- NOT with a recursive Tree-structure
-- 
-- Here I'm trying to build the filesystem Tree-structure
-- It seems to be working
--
-- The sum of sizes of all small directories is: 1543140
-- The size of the directory to be erased is:    1117448
-- 
-- (cl) 2022-12-09  by  Arno Jacobs

module AoC2022_day7_alternative where

import Data.Char
import Data.List
import Data.List.Split


-- Types and data structure
type Name = String
type Size = Int

data FileSystem =   File        Name Size 
                |   Directory   Name Size  [FileSystem]
                    deriving Show


-- Range part 1 & 2
smallSize, diskSpace, updateSpace :: Size
smallSize   =   100000
diskSpace   = 70000000
updateSpace = 30000000

-- Initials and fixed data
--
-- This is the small test file from the AoC-page of day 7
filename :: Name
filename = "../data/inputDay07_2022.txt"

-- Three main commands:
--  1.)     $ cd DirectoryName      -- move 1 directory up into 'DirectoryName'
--  2.)     $ cd ..                 -- move 1 directory down
--  3.)     $ ls                    -- show the files in the current directory
--
cCommand = '$' :: Char              -- commands start at prompt '$'

cBackDir = "$ cd .." :: String      -- complete command string for 'one directory back'

sCD         = "cd" :: String        -- $ cd ~ change directory command
sLS         = "ls" :: String        -- $ ls ~ list directory command
sRootDir    = "/"  :: String        -- identify root directory
sCurrentDir = "."  :: String        -- For 0 sized dot-file
sBackDir    = ".." :: String        -- move back one directory 'name'
space       = " "  :: String        -- split string for the 'splitOn' function
dummy       = "~"  :: String        -- string needed for safe 'splitOn' function

isCommand :: String -> Bool
isCommand (c0:_) = c0 == cCommand

-- Building a filesystem-Tree 
buildTree :: [String] -> [FileSystem]
buildTree []        = []
buildTree (l:ls)  
    | isCommand l && e2 == sCD && e3 == sRootDir = [Directory sRootDir (directorySize bTls) bTls]
    | isCommand l && e2 == sCD && e3 /= sBackDir = [Directory e3 (directorySize bTsdls) bTsdls] ++ bTrdls 
    | isCommand l && e2 == sLS                   = addFiles ls ++ bTnls
    | otherwise                                  = []
        where   
            (e1:e2:e3:_)    = splitOn space (l ++ space ++ dummy)   -- Add dummy so 'e3' is 'filled'
            bTnls           = buildTree $ dropWhile (not . isCommand) ls
            sdls            = subDir ls
            bTls            = buildTree ls
            bTrdls          = buildTree $ drop (length sdls) ls
            bTsdls          = buildTree sdls

-- pick part from 'cd name' to its 'cd ..'  -  and rest of that list
subDir :: [String] -> [String]
subDir ls = take cdBackPos ls
    where 
        cdBackPos = findCDBackIndex 1 0 ls    -- find matching '$ cd ..'
        findCDBackIndex :: Int -> Int -> [String] -> Int
        findCDBackIndex ix cdc []       = ix
        findCDBackIndex ix cdc (s:rls)  
            | backDir s && cdc < 1  = ix
            | backDir s             = findCDBackIndex (ix+1) (cdc-1) rls
            | forwardDir s          = findCDBackIndex (ix+1) (cdc+1) rls
            | otherwise             = findCDBackIndex (ix+1) cdc     rls
        -- Check is command is 'move' one directory back - or forward
        backDir, forwardDir :: String -> Bool
        backDir    cs        = cBackDir == cs 
        forwardDir (_:_:rcs) = sCD      == take 2 rcs

-- Adding the '.' files will make sure that directories with NO file
-- but only sub-directories will have a correct 'size'
addFiles :: [String] -> [FileSystem]
addFiles []                     = [File sCurrentDir 0]
addFiles (l:ls) | isSize l      = [File fName (read fSize)] ++ addFiles ls
                | isCommand l   = [File sCurrentDir 0]
                | otherwise     = addFiles ls
    where
        isSize          = isNumber . head
        (fSize:fName:_) = splitOn space l   

-- Get directory size incl. adding size of all sub-directories 
directorySize :: [FileSystem] -> Size
directorySize fs = sum [ sum (singleDirectorySize sd) | sd <- fs ]
    where
        singleDirectorySize (File _ size)         = [size]
        singleDirectorySize (Directory _ size fs) = [directorySize fs]

-- Reshape to a list of only directorie sizes from the tree
directoriesSizesOnly :: [FileSystem] -> [Size]
directoriesSizesOnly = concat . map getDirectorieSizes
    where
        getDirectorieSizes (Directory _ s fs)    = [s] ++ concat [getDirectorieSizes sd | sd <- fs ]
        getDirectorieSizes (File _ _ )           = []


-- Part 1
-- Filter on all smaller directories and sum all sizes
sumSmallerSizedDirectories :: [Size] -> Size
sumSmallerSizedDirectories = sum . filter (<= smallSize)

-- Part 2 
-- 
-- Calculate the space available and space needed.
-- Find the nearest from the total directory size list. 
findMinimalSpace :: [Size] -> Size
findMinimalSpace dss    | hsl == [] = (-1)      -- (safe) If NO single file is found for erasing
                        | otherwise = head hsl
    where
        findSpace   = updateSpace - (diskSpace - (maximum dss))
        -- find first directory size ~ equal to or just larger than ~ the erase size 
        hsl         = dropWhile (< findSpace) (sort dss)    
        

main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 7  (Haskell)"
            putStrLn "Here with creating and using a recursive tree-structure.\n"
            day7 <- directoriesSizesOnly <$> buildTree <$> lines <$> readFile filename
            putStr   "The sum of sizes of all small directories is: "
            print $ sumSmallerSizedDirectories day7
            putStr   "The size of the directory to be erased is:    "
            print $ findMinimalSpace day7
            putStrLn "0K.\n"
