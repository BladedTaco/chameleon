module Wrasse.Tree where

import Wrasse.Types (ToolInfo)
import Data.Tree (Tree(Node))

import Data.List.Split (splitWhen)
import Util (count)
import Wrasse.Util
import Control.Arrow
import Data.Maybe (fromMaybe)

multiLevel :: [ToolInfo] -> Tree String
multiLevel = Node "Root" . fmap recurse
    where
        recurse (nam, con, out) = Node nam [Node "console" $ layer con, Node "out" $ layer out]

layer :: [String] -> [Tree String]
layer [] = []
layer [x] = [Node (dropWhile (== ' ') x) []]
layer [x1, x2] = if prefixLength x1 == prefixLength x2
    then concatMap (layer . (:[])) [x1, x2]
    else [Node (dropWhile (== ' ') x1) $ layer [x2]]
layer (x1:x2:xs)  =
    uncurry Node . mapTuples <$> 
    extract test (x1:x2:xs)
-- layer (x1:x2:xs) = 
--     uncurry Node <$> 
--     padZipGeneral "" [] 
--     (dropWhile (== ' ') <$> x1 : filter test xn) 
--     (layer <$> splitWhen test xn)
    where
        n = prefixLength x2
        test = (< n) . prefixLength
        xn = x2:xs
        mapTuples :: (Maybe String, [String]) -> (String, [Tree String])
        mapTuples = 
            (dropWhile (== ' ') . fromMaybe "")
            ***
            layer

-- Need to make it check the least indented below the current level
prefixLength :: String -> Int
prefixLength = count (/= ' ')

-- turns a list into a single level tree
extract :: (a -> Bool) -> [a] -> [(Maybe a, [a])]
extract f [] = []
extract f (x:xs) = (
        if f x
        then (Just x, takeWhile (not . f) xs)
        else (Nothing, takeWhile (not . f) $ x:xs)
    ) : extract f (dropWhile (not . f) xs)