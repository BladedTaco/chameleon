module Wrasse.Messages where

import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile, doesDirectoryExist, getDirectoryContents, listDirectory, doesFileExist)
import System.FilePath.Posix (takeDirectory)

import System.IO.Silently

import GHC.IO.Handle
import System.IO

import Wrasse.Types (GHCMessage (GHCMessage))
import Data.List (isPrefixOf, isSuffixOf)
import Control.Exception (try)

import Wrasse.Util


messageHook :: FilePath -> IO [GHCMessage]
messageHook p = do
    files <- loadMessages p
    sequence $ readMessage <$> files


loadMessages :: FilePath -> IO [FilePath]
loadMessages p = do
    dirs <- listDirectory p
    mconcat $ dirMap <$> filter (isPrefixOf "GHC-") dirs
    where
        dirMap m = do
            conts <- listDirectory $ p ++ "/" ++ m
            return $ (mconcat [p, "/", m, "/"] ++) <$> filter (isSuffixOf "index.md") conts


readMessage :: FilePath -> IO GHCMessage
readMessage f = do
    exists <- doesFileExist f
    if exists 
      then do
        contents <- readFile f
        -- let (_:ttl:smry:svry:intr:ext:_:xs) = lines contents
        -- return $ GHCMessage ttl smry svry intr ext xs
        let c = lines contents
        return $ GHCMessage
            (get "title: " c)
            (get "summary: " c)
            (get "severity: " c)
            (get "introduced: " c)
            (get "extension: " c)
            (get "flag: " c)
            (filter (/= "") $ multiple 2 (dropUntil (== "---")) c)
      else do
        return $ GHCMessage  f "" "" "" "" "" [""]
    where
        get k x = mconcat $ drop (length k) <$> filter (isPrefixOf k) x

-- >>> dropUntil (== 5) [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- [6,7,8,9]
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil = drop 1 <..> dropWhile . (not .)

-- >>> multiple 10 (+2) 4
-- 24
multiple :: Int -> (a -> a) -> a -> a
multiple 0 = const id
multiple n = (.) =<< multiple (n - 1)
-- multiple n f = multiple (n-1) f . f
