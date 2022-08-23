{-# LANGUAGE ScopedTypeVariables, PackageImports  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Wrasse.Hook (hook) where

import "ghc" Exception
import GHC

import "ghc" GhcMonad

import GHC.Paths
import "ghc" HscTypes
import System.Environment

import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.FilePath.Posix (takeDirectory)

import System.IO.Silently

import GHC.IO.Handle
import System.IO

import GHC.Generics
import Text.Parsec
import "ghc" Bag ( bagToList )
import "ghc" Outputable ( SDoc(runSDoc), initSDocContext, Outputable (ppr), showSDocUnsafe )

import "ghc" DynFlags
import "ghc" ErrUtils


import GHC.IORef (newIORef, IORef)
import Data.IORef
import HieAst (mkHieFile)

import Language.Haskell.Exts.Lexer

import Language.Haskell.HLint

import Wrasse.Types
import Data.Tree (Tree(..), drawTree)
import Data.List (intercalate)

import Wrasse.Tree (multiLevel)
import "ghc" Util (uncurry3, OverridingBool (Always))
import Control.Lens (traverseOf, Each (each))
import Control.Arrow
import Agda.Utils.Tuple (uncurry4)
import DriverPipeline (preprocess)

import Wrasse.Util

import System.Process 

-- main :: IO ()
-- main = do
--   args <- getArgs :: IO [String]
--   ref <- liftIO $ newIORef ""
--   r <- runGhc (Just libdir) (processGHC ref "Example" (head args))
--   putStrLn "start output"
--   mconcat $ mapTup3 putStrLn  r
--   r' <- readIORef ref
--   putStrLn r'
--   putStrLn "end output"

ghcFile :: String -> FilePath
ghcFile = ("generated/" ++) . (++ ".hs")

-- the entrypoint
hook :: String -> IO WrasseResult
hook f = do
  let m = getModuleName f
  let (modName, file, s) = if m == ""
      then ("Infile", ghcFile "Infile", "module Infile where\n" ++ f)
      else (m, ghcFile "Infile", f)
  createDirectoryIfMissing True $ takeDirectory file
  writeFile file s
  tools <- toolHook modName file
  ghcData <- ghcHook modName file
  return $ WrasseResult tools ghcData "" "" ((, False, -1) <$> multiLevel tools) [] -- (lines $ Data.Tree.drawTree $ filter (/= '\n') <$> multiLevel tools)

--
toolHook :: String -> FilePath -> IO [(String, [(String, [String])])]
toolHook modName file = do
  (GHCResult g1 g2) <- ghcHook modName file
  (h1, h2) <- hlintHook file
  return [
    ("GHC", [ 
      ("output", g1),
      ("code", g2)
    ]),
    ("HLint", [
      ("console", h1),
      ("output", h2)
    ])
    ]

-- runs GHC
ghcHook :: String -> FilePath -> IO GHCResult
ghcHook modName file = do
  let flags = [
          "-fprint-potential-instances"
        , "-ferror-spans"
        , "-Wall"
        ]

  let cmd = "ghc " ++ unwords flags ++ " generated/Infile.hs"
  let sIn = ""

  (_, sOut, sErr) <- readCreateProcessWithExitCode ((shell cmd) {new_session = True, create_group = True}) sIn

  fileContents <- readFile file

  return $ uncurry GHCResult $ mapTup2 lines (sOut ++ "\n" ++ sErr, fileContents)


-- runs HLint
hlintHook ::  FilePath -> IO  ([String], [String])
hlintHook file = do
  ref <- liftIO $ newIORef "" -- make an output IO stream
  -- parse module
  let pflags = defaultParseFlags
  result <- parseModuleEx pflags file Nothing
  let out = processHLint result
  -- hlint
  ideas <- hlint ["generated", "--quiet"]
  ref_out <- readIORef ref -- read the output IO stream
  -- return $ GHCResult (("hlint console: " ++) <$> lines ref_out) $ fmap ("hlint out: " ++) out ++ fmap (("idea: " ++) . show) ideas
  return (lines ref_out, out ++ [""] ++ (show <$> ideas))


moduleParser :: Parsec String () String
moduleParser = do
    string "module "
    res <- many $ noneOf " "
    string " where"
    return res

getModuleName :: String -> String
getModuleName s = case parse moduleParser "" s of
    Left err -> ""
    Right xs -> xs



-- LogAction == DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logHandler :: IORef String -> LogAction
logHandler ref dflags warn severity srcSpan style msg =
  case severity of
     SevError ->  modifyIORef' ref (++ printDoc)
     SevFatal ->  modifyIORef' ref (++ printDoc)
     _        ->  return () -- ignore the rest
  where cntx = initSDocContext dflags style
        locMsg = mkLocMessage severity srcSpan msg
        printDoc = show (runSDoc locMsg cntx)



-- the boilerplate GHC
processHLint :: Either Language.Haskell.HLint.ParseError ModuleEx -> [String]
processHLint e = do
  case e of
    Left pe -> ["Failed compiling"]
    Right me -> ["COMPILED SUCCESSFULLY"]


-- getHieAst modSum env source = mkHieFile modsum env source


cleanGHCOutput :: String -> String
cleanGHCOutput = undefined