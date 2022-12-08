-- Advent of Code 2022 - Day 7 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Because I wanted to fill a Tree-structure (and not a list as done here)
-- which still isn't coded here and because I didn't read the instructions 
-- correctly it took me more than a day too solve this one.
-- 
--
-- The sum of the total sizes of small directories: 1543140
-- The size of the directory to be erased is:       1117448
--
-- (cl) by Arno Jacobs, 2022-12-08

module AoC2022d07ab where

import Data.Char
import Data.List
import Data.List.Split

filename :: String
filename = "data/inputDay07_2022.txt"

cCommand = '$' :: Char  -- commands start at prompt '$'

-- Range part 1
maxSize     =   100000 :: Int
-- Ranges for part 2
diskSpace   = 70000000 :: Int
updateSpace = 30000000 :: Int

-- Types & structure -- In the future try to build a Tree stucture
type Name       = String
type Size       = Int
-- The first  Name will contain the directory
-- The second Name will contain the file name - these are not used in the code 
data FileData   = File Name Name Size   deriving (Eq,Ord,Show)
type FileList   = [FileData]

-- Starting code -- mainly for Part 1
--
-- semi safe -- positive integers only
safeSize :: String -> Size
safeSize s  | isNumber c0   = read s
            | otherwise     = -1
    where c0 = head $ s ++ "."

isCommand :: String -> Bool
isCommand (c0:_) = c0 == cCommand

nextCommand :: [String] -> [String]
nextCommand []      = []
nextCommand (x:xs)  | isCommand x   = (x:xs)
                    | otherwise     = nextCommand xs

-- This will add a sub directory OR step back one sub directory
changeDirectory :: String -> String -> String
changeDirectory cd nd   | nd == ".."    = backdir cd    -- step beck one directory
                        | otherwise     = cd ++ nd ++ "/" 
    where
        backdir = reverse . dropWhile (/='/') . tail . reverse  

-- Only store file names at this stage
-- Directories are stored at 'makeTree' via '$ cd dir'
getFileList :: String -> [String] -> FileList
getFileList _  []       = []
getFileList cd (x:xs)   | isSize            = [File cd name value] ++ getFileList cd xs
                        | isCommand x       = []
                        | otherwise         = getFileList cd xs   
    where
        (size:name:_)   = splitOn " " x
        isSize          = isNumber $ head size
        value           = safeSize size

-- Create a list of all files including its directories.
-- Empty "dot" files are added to directories so directories with NO files
-- and only sub directories will be NOT forgotten when counting directory sizes
makeFilesList :: String -> [String] -> FileList
makeFilesList _ []          = []
makeFilesList cd (x:xs) 
    | isCommand x
    && cmd == "cd"          =   makeFilesList (changeDirectory cd nd) xs
                            --  Add an empty dot-file for empty directories
                            --  There are 3 directories with NO files but with small
                            --  sized directories. So they do count as well. 
    | otherwise             =   [File cd "." 0]   ++            
                                getFileList cd xs ++                -- Add existing files
                                makeFilesList cd (nextCommand xs)   -- work the list
        where 
            (_:cmd:nd:_)    = splitOn " " (x ++ " ?")

-- Sub sum sizes per unique directory
subSum :: FileList -> [Int]
subSum []     = []
subSum (f:fs) = [s + sum rs] ++ subSum rfs
    where
        File d _ s = f
        rs  = map getSize $ filter (sameBaseDirectory d) fs
        rfs = filter (notSameDirectory d) fs
        getSize (File _ _ s) = s
        sameBaseDirectory td (File d _ s) = td == take (length td) d
        notSameDirectory  td (File d _ s) = td /= d 

-- sub sum ALL directories - needed for part 2
allDirectorySizes :: [String] -> [Int]
allDirectorySizes = subSum . makeFilesList "root" 

-- Select via a filter all small directories
smallerSizedDirectories :: [Int] -> [Int]
smallerSizedDirectories = filter (<= maxSize) 


-- Part 2

-- Calculate the space available and space needed.
-- Find the nearest from the total directory size list. 
findMinimalSpace :: [Int] -> Int
findMinimalSpace sizes = head $ dropWhile (< findSpace) $ sort sizes
    where
        findSpace = updateSpace - (diskSpace - (maximum sizes))


main :: IO ()
main = do   putStrLn "Advent of Code 2022 - day 7  (Haskell)"
            day7 <- allDirectorySizes <$> lines <$> readFile filename
            let ssdl = smallerSizedDirectories day7
            putStr   "The sum of sizes of all small directories is: "
            print $ sum ssdl
            putStr   "The size of the directory to be erased is:    "
            print $ findMinimalSpace day7
            putStrLn "0K.\n"

