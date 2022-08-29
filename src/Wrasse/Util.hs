module Wrasse.Util where

import Data.Map
import Control.Arrow
import Control.Monad
import Data.Char (isSpace)

-- >>> trim " abc "
-- "abc"
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- >>> dropUntil (== 5) [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- [6,7,8,9]
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil = Prelude.drop 1 <..> dropWhile . (not .)

-- >>> multiple 10 (+2) 4
-- 24
multiple :: Int -> (a -> a) -> a -> a
multiple 0 = const id
multiple n = (.) =<< multiple (n - 1)
-- multiple n f = multiple (n-1) f . f

mapTup2 :: (t -> c) -> (t, t) -> (c, c)
mapTup2 = join (***)

mapTup3 :: (t -> c) -> (t, t, t) -> (c, c, c)
mapTup3 f (a, b, c) = (f a, f b, f c)


-- | Nested map
--
-- >>> (<$$>) head [[[1],[2,3,4]],[[5,6],[7]]]
-- [[1,2],[5,7]]
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)



-- | zips two lists together, using default values for holes
--
-- >>> padZipGeneral 0 10 [5..9] [1..5]
-- [(5,1),(6,2),(7,3),(8,4),(9,5)]
--
padZipGeneral :: a -> b -> [a] -> [b] -> [(a, b)]
padZipGeneral a b (c:cs) (d:ds)  = (c, d) : padZipGeneral a b cs ds
padZipGeneral a _  []      d     = zip (repeat a) d
padZipGeneral _ b   c      []    = zip c (repeat b)



{-
    Combinator Utilities --------------------------------------------------------------------------
-}

-- | checks && over a fanout transformation
--
-- >>> ((1 <) <&&> (< 10)) <$> [0, 5, 10]
-- [False,True,False]
--
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = uncurry (&&) <...> (&&&)

infixr 4 <&&>

-- | checks || over a fanout transformation
--
-- >>> ((1 <) <||> (> 10)) <$> [0, 5, 10]
-- [False,True,True]
--
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = uncurry (||) <...> (&&&)

infixr 4 <||>


-- | Doubly nested map
--
-- >>> (<$$$>) head [[[[1, 2], [3, 4]]], [[[5, 6], [7, 8]]]]
-- [[[1,3]],[[5,7]]]
--
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = (<$$>) . (<$>)

infixr 4 <$$$>

-- | Performs lookups over a iterable
--
-- >>> fromList [(0, "zero"),(1, "one"),(2, "two")] <$!> [0,1,2,0,1,2,0,1,2]
-- ["zero","one","two","zero","one","two","zero","one","two"]
--
(<$!>) :: (Ord key, Functor f) => Map key val -> f key -> f val
(<$!>) = (<$>) . (!)

infixr 9 <$!>

-- | Map over the result of a function waiting on 1 argument
--
-- equivalent to (f1 <$>) . f2
--
-- >>> ((+1) <$.> tail) [1, 2, 3]
-- [3,4]
--
(<$.>) :: (Functor f) => (b -> c) -> (a -> f b) -> a -> f c
(<$.>) = (.) . (<$>)

infixr 9 <$.>

-- | Map over the result of a function waiting on 2 arguments
--
-- Equivalent to (f1 <$>) <..> f2
--
-- >>> (snd <$..> zip) [1, 2, 3] [4, 5, 6]
-- [4,5,6]
--
(<$..>) :: Functor f => (b -> c) -> (a1 -> a2 -> f b) -> a1 -> a2 -> f c
(<$..>) = (<..>) . (<$>)

infixr 9 <$..>

-- | map with a function waiting on 1 argument
--
-- Equivalent to f1 . (f2 <$>)
--
-- >>> (tail <.$> (1-)) [1, 2, 3]
-- [-1,-2]
--
(<.$>) :: (Functor f) => (f b -> c) -> (a1 -> b) -> f a1 -> c
(<.$>) = (<..> fmap)

infixr 9 <.$>


-- | . but for 2 arguments
--
-- Equivalent to (f1 .) . f2
--
-- >>> (head <..> zip) [1, 2, 3] [4, 5, 6]
-- (1,4)
--
(<..>) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(<..>) = (.) . (.)

infixr 9 <..>

-- | . but for 3 arguments
--
-- Equivalent to ((f1 .) .) . f2
--
-- >>> (tail <...> ((++) <..> (++))) [1, 2, 3] [4, 5, 6] [7, 8, 9]
-- [2,3,4,5,6,7,8,9]
--
(<...>) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
(<...>) = (.) . (<..>)

infixr 9 <...>

-- | . but for 4 arguments
--
-- Equivalent to (((f1 .) .) .) . f2
--
-- >>> (tail <....> ((++) <...> (++) <..> (++))) [1, 2, 3] [4, 5, 6] [7, 8, 9] [10, 11, 12]
-- [2,3,4,5,6,7,8,9,10,11,12]
--
(<....>) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> c
(<....>) = (.) . (<...>)

infixr 9 <....>

-- | . but for 5 arguments
--
-- Equivalent to ((((f1 .) .) .) .) . f2
--
-- >>> (tail <.....> ((++) <....> (++) <...> (++) <..> (++))) [1, 2, 3] [4, 5, 6] [7, 8, 9] [10, 11, 12] [13, 14, 15]
-- [2,3,4,5,6,7,8,9,10,11,12,13,14,15]
--
(<.....>) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> c
(<.....>) = (.) . (<....>)

infixr 9 <.....>

-- | . but for six arguments
--
-- Equivalent to (((((f1 .) .) .) .) .) . f2
--
-- >>> (tail <......> ((++) <.....> (++) <....> (++) <...> (++) <..> (++))) [1, 2, 3] [4, 5, 6] [7, 8, 9] [10, 11, 12] [13, 14, 15] [16, 17, 18]
-- [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]
--
(<......>) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> c
(<......>) = (.) . (<.....>)

infixr 9 <......>



------------------------------ Combinator Functions



-- >>> let f = id <$$.> replicate 5
-- >>> f [[1, 2]]
-- [[[1,2]],[[1,2]],[[1,2]],[[1,2]],[[1,2]]]
--
(<$$.>) :: (Functor f, Functor g) => (b -> c) -> (f a -> f (g b)) -> f a -> f (g c)
(<$$.>) = (.) . (<$>) . (<$>)

infixr 9 <$$.>

-- | map over the result of a function waiting on 3 arguments
--
-- Equivalent to (f1 <$>) <...> f2
--
-- >>> (uncurry3 ((+) <..> (+)) <$...> zip3) [1, 2, 3] [4, 5, 6] [7, 8, 9]
-- [12,15,18]
--
(<$...>) :: Functor f => (b -> c) -> (a1 -> a2 -> a3 -> f b) -> a1 -> a2 -> a3 -> f c
(<$...>) = (<...>) . (<$>)

infixr 9 <$...>

-- | map with a function waiting on 2 arguments
--
-- Equivalent to f1 <..> liftM2 f2
--
-- >>> (tail <..$> (,)) [1, 2, 3] [3, 2, 1]
-- [(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
--
-- (<..$>) :: Functor f => (a1 -> a2 -> f b) -> (b -> c) -> a1 -> a2 -> f c
(<..$>) :: (Monad f) => (f b -> c) -> (a1 -> a2 -> b) -> f a1 -> f a2 -> c
(<..$>) = (<...> liftM2)

infixr 9 <..$>

-- | map with a function waiting on 3 arguments
--
-- Equivalent to f1 <...> liftM3 f2
--
-- >>> (tail <...$> (,,)) [1, 2] [4, 5] [7, 8]
-- [(1,4,8),(1,5,7),(1,5,8),(2,4,7),(2,4,8),(2,5,7),(2,5,8)]
--
(<...$>) :: (Monad f) => (f b -> c) -> (a1 -> a2 -> a3 -> b) -> f a1 -> f a2 -> f a3 -> c
(<...$>) = (<....> liftM3)

infixr 9 <...$>

-- | map with the left function waiting on 1 argument over the result of the right function waiting on 1 argument
--
-- Equivalent to f1 a . (f2 <$>)
--
-- >>> ((+) <.$.> tail) 1 [1, 2, 3]
-- [3,4]
--
(<.$.>) :: Functor f => (a -> c -> d) -> (b -> f c) -> a -> b -> f d
-- (<.$.>) = flip $ (.) . flip (<$.>)
-- (<.$.>) f g a = ((f a <$.> g))
(<.$.>) = flip . ((<$.>) .)

infixr 9 <.$.>

-- | map with the left function waiting on 1 argument over the result of the right function waiting on 2 arguments
--
-- Equivalent to f1 a <..> (f2 <$>)
--
-- >>> ((+) <..$.> (++)) 1 [1, 2, 3] [4, 5, 6]
-- [2,3,4,5,6,7]
--
--
(<..$.>) :: Functor f => (a -> c -> d) -> (b1 -> b2 -> f c) -> a -> b1 -> b2 -> f d
-- (<..$.>) = flip ((f <.$.>) . g)
(<..$.>) = flip <..> (.) . (<.$.>)

infixr 9 <..$.>

-- | map with the left function waiting on 2 arguments over the result of the right function waiting on 1 argument
--
-- Equivalent to f1 a b . (f2 <$>)
--
-- >>> (((*) <..> (+)) <.$..> tail) 1 2 [1, 2, 3] 
-- [6,9]
--
(<.$..>) :: Functor f => (a1 -> a2 -> c -> d) -> (b1 -> f c) -> a1 -> a2 -> b1 -> f d
-- (<.$..>) f g a b  =  (f a b <$.> g )
-- (<.$..>) = flip ((.) . flip (<.$.>)) <-- unreadable mess.
(<.$..>) f g = (<.$.> g) . f

infixr 9 <.$..>


-- | map with the left function waiting on 3 arguments over the result of the right function waiting on 1 argument
--
-- Equivalent to f1 a b c . (f2 <$>)
--
-- >>> (((+) <...> (+) <..> (+)) <.$...> tail) 1 2 3 [1, 2, 3] 
-- [8,9]
--
(<.$...>) :: Functor f => (a1 -> a2 -> a3 -> c -> d) -> (b1 -> f c) -> a1 -> a2 -> a3 -> b1 -> f d
(<.$...>) f g = (<.$..> g) . f

infixr 9 <.$...>

-- | map with the left function waiting on 1 argument over the result of the right function waiting on 3 arguments
--
-- Equivalent to f1 a <...> (f2 <$>)
--
-- >>> ((+) <...$.> ((++) <..> (++))) 1 [1, 2, 3] [4, 5, 6] [7, 8, 9]
-- [2,3,4,5,6,7,8,9,10]
--
(<...$.>) :: Functor f => (a -> c -> d) -> (b1 -> b2 -> b3 -> f c) -> a -> b1 -> b2 -> b3 -> f d
(<...$.>) = flip <..> (.) . (<..$.>)

infixr 9 <...$.>
