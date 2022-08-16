module Wrasse.Messages where

import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile, doesDirectoryExist, getDirectoryContents, listDirectory, doesFileExist)
import System.FilePath.Posix (takeDirectory)

import System.IO.Silently

import GHC.IO.Handle
import System.IO

import Wrasse.Types (GHCMessage (GHCMessage), def)
import Wrasse.Instance
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
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
        let [_, e, _] = splitOn "/" f
        return $ GHCMessage
            e
            (get "title: " c)
            (get "summary: " c)
            (get "severity: " c)
            (get "introduced: " c)
            (get "removed: " c)
            (get "extension: " c)
            (get "flag: " c)
            (filter (/= "") $ multiple 2 (dropUntil (== "---")) c)
            def -- GHCExample
      else do
        return def
    where
        get k x = mconcat $ drop (length k) <$> filter (isPrefixOf k) x
