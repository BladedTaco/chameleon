module Task1 where

-- Add all the natural numbers below 1000
-- that are multiples of 3 or 5.

-- This comment contains â®bidirectional formatâ¬ chars

-- add :: a -> a -> a
-- add x y = x + y


sum [] = 0
sum [x] = x
sum (x:xs) = x + Task1.Task1.Task1.sum xs

check (x:xs)
  | x `mod` 3 == 0 || x `mod` 5 == 0 = x + check xs
  | otherwise = check xs

problem_1 = Task1.Task1.Task1.sum (check [1..999])
