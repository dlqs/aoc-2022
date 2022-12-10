module Main where

import Data.List (sort)
import GHC.IO.Handle (isEOF)

-- accum, original string, (processed string without empty line, remaining string)
takeWhileNotEmptyLine :: String -> String -> (String, String)
takeWhileNotEmptyLine acc s =
  case s of
    '\n' : '\n' : xs -> (acc, xs)
    x : xs -> takeWhileNotEmptyLine (acc ++ [x]) xs
    "" -> (acc, "")

-- add together lines of integers
sumLines x = sum $ map (read :: String -> Int) (lines x)

-- line = "hello\n\nbye\n\nhellos"
-- line = "ab\n\nc"

data Top3 = Top3
  { a :: !Int,
    b :: !Int,
    c :: !Int
  }
  deriving (Show)

max3 :: Top3 -> Int -> Top3
max3 Top3 {a = a, b = b, c = c} d =
  let a' : b' : c' : d' = drop 1 $ sort [a, b, c, d]
   in Top3 {a = a', b = b', c = c'}

process top3SoFar contents = do
  let (x, xs) = takeWhileNotEmptyLine "" contents
  case x of
    "" -> top3SoFar
    x -> process (max3 top3SoFar (sumLines x)) xs

main :: IO ()
main = do
  contents <- getContents
  let top3 = process Top3 {a = 0, b = 0, c = 0} contents
   in print $ a top3 + b top3 + c top3
