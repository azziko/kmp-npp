import Data.Array
import Data.List
import System.IO
import Control.Monad
import System.Environment 

buildTable :: String -> Array Int Int
buildTable [] = array (0, 0) []
buildTable needle = listArray (0, n - 1) table
    where
        n = length needle
        table = 0 : build 1 0

        build i j
            | i > n - 1 = []
            | needle !! i == needle !! j = 
                (j + 1) : build (i + 1) (j + 1)
            | j > 0 = build i (table !! (j - 1))
            | otherwise = 0 : build (i + 1) 0

-- runKMP :: String -> [String] -> Bool
-- runKMP needle lines = True

searchInLine :: String -> String -> [Int]
searchInLine needle haystack = search 0 0 []
    where
        n = length needle
        m = length haystack
        table = buildTable needle

        search i j acc
            | i >= m = reverse acc
            | haystack !! i == needle !! j =
                if j == n - 1
                    then search (i + 1) (table ! j) ((i - j) : acc)
                    else search (i + 1) (j + 1) acc
            | j > 0 = search i (table ! (j - 1)) acc
            | otherwise = search (i + 1) 0 acc

main = do
    (needle : filename : args) <- getArgs

    contents <- readFile filename
    print (searchInLine needle (head (lines contents)))