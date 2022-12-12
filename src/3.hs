module Main where

import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)

score a
  | ord a >= 97 = ord a - 96
  | otherwise = ord a - 38

priority :: String -> Int
priority line =
  let (l1, l2) = splitAt (length line `div` 2) line
   in score $ head $ l1 `intersect` l2

part1 = do
  contents <- getContents
  print $ sum $ map priority $ lines contents

scoreGroup :: [String] -> Int
scoreGroup group =
  let l1 : l2 : l3 : _ = group
   in score $ head $ l1 `intersect` l2 `intersect` l3

main =
  do content <- getContents
     let groupsOfThree = chunksOf 3 (lines content)
     print $ foldl (\a group -> a + scoreGroup group) 0 groupsOfThree