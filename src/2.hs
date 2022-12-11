module Main where

data Outcome = Won | Draw | Lost deriving (Show, Eq)

data Move = Rock | Paper | Scissors deriving (Show, Eq)

score :: [Char] -> Integer
score
  xs =
    outcomeScore o + moveScore (getMove p1 o)
    where
      p1 : o : _ = xs
      outcomeScore o = case o of
        'X' -> 0
        'Y' -> 3
        'Z' -> 6
        _ -> 0
      moveScore move = case move of
        Rock -> 1
        Paper -> 2
        Scissors -> 3
      getMove p o = case p of
        'A' -> case o of
          'X' -> Scissors
          'Y' -> Rock
          'Z' -> Paper
          _ -> Rock
        'B' -> case o of
          'X' -> Rock
          'Y' -> Paper
          'Z' -> Scissors
          _ -> Rock
        'C' -> case o of
          'X' -> Paper
          'Y' -> Scissors
          'Z' -> Rock
          _ -> Rock
        _ -> Rock

main = do
  contents <- getContents
  let parsed = map (map head . words) (lines contents)
  let total = sum $ map score parsed
  print total
