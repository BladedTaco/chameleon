
{-# LANGUAGE PackageImports #-}

module Wrasse.Tree where

import Wrasse.Types ( ToolInfo ) 
import Data.Tree ( Tree(Node) ) 

import Wrasse.Util ( (<||>) ) 
import Control.Arrow ( Arrow((***)) ) 
import Data.Maybe ( fromMaybe ) 
import "ghc" Util ( singleton )


-- turns a list of toolinfo into the wrasse output tree
multiLevel :: [ToolInfo] -> Tree String
multiLevel = Node "Root" . fmap recurse
    where
        -- recurse down toolInfo list, deciding what nodes are children, and which are siblings
        recurse (nam, subs) = Node nam $ uncurry ((. layer) . Node) <$> subs


-- layer a list of strings into a list of neighbour trees based on indentation
layer :: [String] -> [Tree String]
layer [] = [] -- empty to empty
layer [x] = [Node (dropWhile (== ' ') x) []] -- singleton to singleton with whitespace stripped from prefix
layer [x1, x2] = if prefixLength x1 == prefixLength x2 -- two element goes to either sibling or [parent, child]
    -- sibling
    then concatMap (layer . singleton) [x1, x2]
    -- parent, child
    else [Node (dropWhile (== ' ') x1) $ layer [x2]]
-- three or more elements takes into account the [root, indent, root] case
layer (x1:x2:xs) = uncurry Node . mapTuples <$> extract isRootLevel (x1:x2:xs)
    where
        -- if the current node is a root level sibling in the current recursion
        isRootLevel = ((< prefixLength x2) <||> (== prefixLength x1)) . prefixLength
        -- recursively map children, dropping any leading spaces for each returned node
        mapTuples :: (Maybe String, [String]) -> (String, [Tree String])
        mapTuples = (dropWhile (== ' ') . fromMaybe "") *** layer
        

-- get the number of spaces prefixing the string
-- >>> prefixLength "   .  test      "
-- 3
prefixLength :: String -> Int
prefixLength = length . takeWhile (== ' ')


-- turns a list into a single level forest (list of trees), in the form of [(Nothing, children), (Just Parent, children)]
-- >>> extract (== 1) [0, 1, 2, 3, 1, 2, 3, 12, 1, 1, 2, 3]
-- [(Nothing,[0]),(Just 1,[2,3]),(Just 1,[2,3,12]),(Just 1,[]),(Just 1,[2,3])]
-- 
extract :: (a -> Bool) -> [a] -> [(Maybe a, [a])]
extract f [] = [] -- empty list base case
extract f (x:xs) = (
        -- if test is passed
        if f x
        --  parent and children
        then (Just x, childs)
        -- only children
        else (Nothing, x:childs)
    -- recurse for remainder of list
    ) : extract f siblings
    where
        -- childs = tail prefix that fails predicate, siblings = remainder
        (childs, siblings) = break f xs
