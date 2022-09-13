module Wrasse.Messages where

import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile, doesDirectoryExist, getDirectoryContents, listDirectory, doesFileExist)
import System.FilePath.Posix (takeDirectory)

import System.IO.Silently

import GHC.IO.Handle
import System.IO

import Wrasse.Types (GHCMessage (GHCMessage), def, GHCExample (GHCExample))
import Wrasse.Instance
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Control.Exception (try, IOException, catch)

import Wrasse.Util
import Control.Monad (filterM, (<=<), join)
import System.FilePath ((</>))
import Agda.Compiler.Backend (TCErr(IOException))
import Data.Maybe (fromMaybe)


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
            conts <- listDirectory $ p </> m
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
        let [pathTo, e, _] = splitOn "/" f
        examp <- loadExamples (pathTo </> e)
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
            examp
      else do
        return def
    where
        get k x = mconcat $ drop (length k) <$> filter (isPrefixOf k) x



loadExamples :: FilePath -> IO [GHCExample]
loadExamples p = do
    dirs <- filterM (doesDirectoryExist . (p </>)) =<< listDirectory p
    let dirs_ = (p </>) <$> dirs
    mapM readExample dirs_


readExample :: FilePath -> IO GHCExample
readExample f = do
    before <- findF "before"
    after <- findF "after"
    index <- maybeReadFile $ f </> "index.md"

    return $ GHCExample (get "title: " $ lines index) [] "" before after
    where
        get k x = mconcat $ drop (length k) <$> filter (isPrefixOf k) x
        findF = sequence <=< maybeReadFile <$$$> (listDirectoryFull . (f </>))


-- maybeReadFile :: FilePath -> IO (Maybe String)
-- maybeReadFile f = (Just <$> readFile f) `catch` handler
--     where
--         handler :: IOException -> IO (Maybe String)
--         handler _ = pure Nothing

maybeReadFile :: FilePath -> IO String
maybeReadFile f = readFile f `catch` handler
    where
        handler :: IOException -> IO String
        handler _ = return ""

listDirectoryFull :: FilePath -> IO [FilePath]
listDirectoryFull f = (f </>) <$$> listDirectory f `catch` handler
    where
        handler :: IOException -> IO [FilePath]
        handler _ = return []

