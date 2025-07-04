import Data.Array
import Data.Char
import Data.List
import System.IO
import Control.Monad
import System.Environment

buildTable :: String -> Array Int Int
buildTable [] = array (0, 0) []
buildTable needle = table
    where
        n = length needle
        table = listArray (0, n - 1) (0:build 1 0)
        needleArr = listArray (0, n - 1) needle

        build i j
            | i > n - 1 = []
            | needleArr ! i == needleArr ! j =
                (j + 1) : build (i + 1) (j + 1)
            | j > 0 = build i (table ! (j - 1))
            | otherwise = 0 : build (i + 1) 0

searchInLine :: Array Int Int -> String -> String -> [Int]
searchInLine table needle haystack = search 0 0 []
    where
        n = length needle
        m = length haystack
        needleArr = listArray (0, n - 1) needle
        haystackArr = listArray (0, m - 1) haystack

        search i j acc
            | i >= m = reverse acc
            | haystackArr ! i == needleArr ! j =
                if j == n - 1
                    then search (i + 1) (table ! j) ((i - j) : acc)
                    else search (i + 1) (j + 1) acc
            | j > 0 = search i (table ! (j - 1)) acc
            | otherwise = search (i + 1) 0 acc

highlightMatch :: [Int] -> Int -> String
highlightMatch [] _ = []
highlightMatch pos len = result
    where
        result = [doesMatch i | i <- [0 .. maximum pos + len]]

        doesMatch i = if any (\pos -> i >= pos && i < pos + len) pos
            then '^'
            else ' '

runKMP :: String -> [String] -> IO ()
runKMP needle lines = mapM_ printMatch (zip [1..] lines)
    where
        table = buildTable needle

        printMatch (num, line) =
            let pos = searchInLine table needle (map toLower line)
            in
                (unless (null pos) $ do
                        putStrLn $ show num ++ ": " ++ line
                        putStrLn $ replicate (length (show num) + 2) ' ' ++ highlightMatch pos (length needle))

main = do
    args <- getArgs

    case args of
        (needle : filename : _) -> do
            contents <- readFile filename
            runKMP (map toLower needle) (lines contents)
        _ -> do
            putStrLn "To run the program, provide all the necessary arguments: kmp <needle> <filename>"