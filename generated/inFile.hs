module Task1 where

-- Takes the first n elements from a list
take' :: Int -> [Int] -> [Int]
take' n [] = []
take' n (x:xs) = x ++ take' (n - 1) xs
