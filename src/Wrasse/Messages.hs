module Wrasse.Messages where

-- import System directory handling functions
import System.Directory ( doesDirectoryExist, doesFileExist, listDirectory ) 
import System.FilePath ( (</>) ) 

-- import Data functions
import Data.List ( isPrefixOf, isSuffixOf ) 
import Data.List.Split ( splitOn ) 

-- import Control functions
import Control.Exception ( IOException, catch ) 
import Control.Monad ( (<=<), filterM ) 

-- import Wrasse functions and instances
import Wrasse.Instance ()
import Wrasse.Types ( Default(def), GHCExample(GHCExample), GHCMessage(GHCMessage) ) 
import Wrasse.Util ( (<$$$>), (<$$>), dropUntil, multiple ) 

-- main hook for loading HEMI database from given filepath
messageHook :: FilePath -> IO [GHCMessage]
messageHook p = do
    -- load the message index filepaths
    files <- loadMessages p
    -- load and return the message contents
    sequence $ readMessage <$> files

-- gets the filepath of each error code from the database filepath
loadMessages :: FilePath -> IO [FilePath]
loadMessages p = do
    -- list the contents of the current directory
    dirs <- listDirectory p
    -- turn all subfolders into a list of all full filepaths for index files
    mconcat $ dirMap <$> filter (isPrefixOf "GHC-") dirs
    where
        -- maps directory to full filepaths
        dirMap m = do
            -- list directory contents as full filepath
            conts <- listDirectory $ p </> m
            -- return full filename of all index.md files
            return $ (mconcat [p, "/", m, "/"] ++) <$> filter (isSuffixOf "index.md") conts

-- reads a single error code message into a GHCMessage
readMessage :: FilePath -> IO GHCMessage
readMessage f = do
    -- ensure file exists
    exists <- doesFileExist f
    if exists
      then do
        -- read file contents as lines
        contents <- readFile f
        let c = lines contents
        -- get filepath and error code
        let [pathTo, e, _] = splitOn "/" f
        -- load all examples for error
        examp <- loadExamples (pathTo </> e)
        -- return data
        return $ GHCMessage
            e -- error code
            -- parse each aspect
            (get "title: " c)
            (get "summary: " c)
            (get "severity: " c)
            (get "introduced: " c)
            (get "removed: " c)
            (get "extension: " c)
            (get "flag: " c)
            -- return rest of unparsed file
            (filter (/= "") $ multiple 2 (dropUntil (== "---")) c)
            -- and examples
            examp
      else do
        -- file doesn't exist, so return default object
        return def
    where
        -- parses token string, returning contents
        get k x = mconcat $ drop (length k) <$> filter (isPrefixOf k) x


-- loads each example nested in an error code
loadExamples :: FilePath -> IO [GHCExample]
loadExamples p = do
    -- get directories of all examples for error
    dirs <- filterM (doesDirectoryExist . (p </>)) =<< listDirectory p
    -- append filepath to start
    let dirs_ = (p </>) <$> dirs
    -- read and return all examples
    mapM readExample dirs_

-- reads a single example into a GHCExample
readExample :: FilePath -> IO GHCExample
readExample f = do
    -- read before code, after code, and index file
    before <- findF "before"
    after <- findF "after"
    index <- maybeReadFile $ f </> "index.md"

    -- return parsed example
    return $ GHCExample (get "title: " $ lines index) [] "" before after
    where
        -- parse a string token with string contents
        get k x = mconcat $ drop (length k) <$> filter (isPrefixOf k) x
        -- read files in directory f, with failure returning empty string
        findF = sequence <=< maybeReadFile <$$$> (listDirectoryFull . (f </>))

-- safe file reading with exception catching
maybeReadFile :: FilePath -> IO String
maybeReadFile f = readFile f `catch` handler
    where
        handler :: IOException -> IO String
        handler _ = return ""

-- safe directory listing with full path returned instead of just new directories
listDirectoryFull :: FilePath -> IO [FilePath]
listDirectoryFull f = (f </>) <$$> listDirectory f `catch` handler
    where
        handler :: IOException -> IO [FilePath]
        handler _ = return []

