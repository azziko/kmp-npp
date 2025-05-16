import Data.Array
import Data.List

buildTable :: String -> Array Int Int
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