{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Wrasse.Hook (hook) where

import Exception
import GHC

import GhcMonad

import GHC.Paths
import HscTypes
import System.Environment

import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.FilePath.Posix (takeDirectory)

import System.IO.Silently

import GHC.IO.Handle
import System.IO

import GHC.Generics
import Text.Parsec
import Bag ( bagToList )
import Outputable ( SDoc(runSDoc), initSDocContext, Outputable (ppr), showSDocUnsafe )

import DynFlags
import ErrUtils


import GHC.IORef (newIORef, IORef)
import Data.IORef
import HieAst (mkHieFile)

import Language.Haskell.Exts.Lexer

import Language.Haskell.HLint

import Wrasse.Types
import Data.Tree (Tree(..), drawTree)
import Data.List (intercalate)

import Wrasse.Tree (multiLevel)

main :: IO ()
main = do
  args <- getArgs :: IO [String]
  ref <- liftIO $ newIORef ""
  r <- runGhc (Just libdir) (processGHC ref "Example" (head args))
  putStrLn "start output"
  mconcat $ putStrLn <$> r
  r' <- readIORef ref
  putStrLn r'
  putStrLn "end output"

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
  return $ WrasseResult tools "" "" "" (multiLevel tools) (lines $ Data.Tree.drawTree $ filter (/= '\n') <$> multiLevel tools)

--
toolHook :: String -> FilePath -> IO [(String, [String], [String])]
toolHook modName file = do
  ghc <- ghcHook modName file
  hlint <- hlintHook file
  return [
    ("GHC", fst ghc, snd ghc),
    ("HLint", fst hlint, snd hlint)
    ]

-- runs GHC
ghcHook :: String -> FilePath -> IO ([String], [String])
ghcHook modName file = do
  ref <- liftIO $ newIORef "" -- make an output IO stream
  result <- runGhc (Just libdir) (processGHC ref modName file)
  ref_out <- readIORef ref -- read the output IO stream
  -- return $ GHCResult (("ghc console: " ++) <$> lines ref_out) (fmap ("ghc result: " ++) result)
  return (lines ref_out, intercalate ["", "\n", ""] $ lines <$> result)

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
processGHC :: IORef String -> String -> FilePath -> Ghc [String]
processGHC ref moduleName path = do
  dflags <- getSessionDynFlags
  -- ref <- liftIO $ newIORef ""
  let dflags' = dflags {
      hscTarget = HscNothing,
      ghcLink = NoLink,
      log_action = logHandler ref,
      maxValidHoleFits = Nothing,
      refLevelHoleFits = Nothing,
      maxRefHoleFits = Nothing,
      maxRelevantBinds = Nothing
    }
  setSessionDynFlags dflags'
  let mn = mkModuleName moduleName
  let hsTarketId = TargetFile path Nothing
  addTarget
    Target
      { targetId = hsTarketId,
        targetAllowObjCode = False,
        targetContents = Nothing
      }

  eitherl <- gtry (load LoadAllTargets) :: Ghc (Either SourceError SuccessFlag)
  case eitherl of
    Left se -> do
      removeTarget hsTarketId
      return ["Failed at stage: loading", show se]
    Right sf -> do
      modSum <- getModSummary mn
      eitherp <- gtry (parseModule modSum) :: Ghc (Either SourceError ParsedModule)
      case eitherp of
        Left se -> do
          return ["Failed at stage: parsing", show se]
        Right p -> do
          t <- gtry (typecheckModule p) :: Ghc (Either SourceError TypecheckedModule)
          case t of
            Left se -> do
              let ParsedModule _ ps imprts anns = p
              return ["Failed at stage: type checking", show se, showSDocUnsafe $ ppr ps]
            Right tc -> do
              let TypecheckedModule _ (Just rs) ts modInfo (typeGlobalEnv, moduleDeets) = tc
              let hieFile = mkHieFile modSum typeGlobalEnv rs


              return ["Program looks good"]


-- the boilerplate GHC
processHLint :: Either Language.Haskell.HLint.ParseError ModuleEx -> [String]
processHLint e = do

  case e of
    Left pe -> ["Failed compiling"]
    Right me -> ["COMPILED SUCCESSFULLY"]


-- getHieAst modSum env source = mkHieFile modsum env source


cleanGHCOutput :: String -> String
cleanGHCOutput = undefined