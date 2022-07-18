module Wrasse.Tree where

import Wrasse.Types (ToolInfo)
import Data.Tree (Tree(Node))

import Data.List.Split (splitWhen)
import Util (count, singleton)
import Wrasse.Util
import Control.Arrow
import Data.Maybe (fromMaybe)

multiLevel :: [ToolInfo] -> Tree String
multiLevel = Node "Root" . fmap recurse
    where
        recurse (nam, subs) = Node nam $ uncurry ((. layer) . Node) <$> subs
        -- recurse (nam, con, out) = Node nam [Node "console" $ layer con, Node "out" $ layer out]

layer :: [String] -> [Tree String]
layer [] = []
layer [x] = [Node (dropWhile (== ' ') x) []]
layer [x1, x2] = if prefixLength x1 == prefixLength x2
    then concatMap (layer . singleton) [x1, x2]
    else [Node (dropWhile (== ' ') x1) $ layer [x2]]
layer (x1:x2:xs) = uncurry Node . mapTuples <$> extract isRootLevel (x1:x2:xs)
    where
        isRootLevel = ((< prefixLength x2) <||> (== prefixLength x1)) . prefixLength
        mapTuples :: (Maybe String, [String]) -> (String, [Tree String])
        mapTuples = (dropWhile (== ' ') . fromMaybe "") *** layer
-- layer (x1:x2:xs) = 
--     uncurry Node <$>  padZipGeneral "" [] 
--     (dropWhile (== ' ') <$> x1 : filter test xn) 
--     (layer <$> splitWhen test xn)

-- Need to make it check the least indented below the current level
-- >>> prefixLength "   .  test      "
-- 3
prefixLength :: String -> Int
prefixLength = length . takeWhile (== ' ')

-- turns a list into a single level tree
-- >>> extract (== 1) [0, 1, 2, 3, 1, 2, 3, 12, 1, 1, 2, 3]
-- [(Nothing,[0]),(Just 1,[2,3]),(Just 1,[2,3,12]),(Just 1,[]),(Just 1,[2,3])]
-- 
extract :: (a -> Bool) -> [a] -> [(Maybe a, [a])]
extract f [] = []
extract f (x:xs) = (
        if f x
        then (Just x, childs)
        else (Nothing, x:childs)
    ) : extract f siblings
    where
        (childs, siblings) = break f xs
