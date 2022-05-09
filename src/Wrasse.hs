{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveGeneric #-}

module Wrasse where

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
import Bag
import Outputable

import DynFlags
import ErrUtils 


import GHC.IORef (newIORef, IORef)
import Data.IORef

data GHCResult
  = GHCResult
      { 
        console :: String,
        payload :: String
      }
      deriving (Show, Generic)

main :: IO ()
main = do
  args <- getArgs :: IO [String]
  ref <- liftIO $ newIORef ""
  r <- runGhc (Just libdir) (process ref "Example" (head args))
  putStrLn "start output"
  putStrLn r
  r' <- readIORef ref
  putStrLn r
  putStrLn "end output"

ghcFile :: String -> FilePath
ghcFile = ("generated/" ++) . (++ ".hs")

catchOutput :: IO () -> IO String
catchOutput f = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  f
  hDuplicateTo stdout_dup stdout
  str <- readFile tmpf
  removeFile tmpf
  return str

-- the entrypoint
hook :: String -> IO GHCResult
hook f = do
  let m = getModuleName f
  let (modName, file, s) = if m == ""
      then ("Infile", ghcFile "Infile", "module Infile where\n" ++ f)
      else (m, ghcFile "Infile", f)
  createDirectoryIfMissing True $ takeDirectory file
  writeFile file s
  ref <- liftIO $ newIORef "" -- make an output IO stream
  result <- runGhc (Just libdir) (process ref modName file)
  ref_out <- readIORef ref -- read the output IO stream
  return $ GHCResult ref_out result

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
process :: IORef String -> String -> FilePath -> Ghc String
process ref moduleName path = do
  dflags <- getSessionDynFlags
  -- ref <- liftIO $ newIORef ""
  let dflags' = dflags {
      hscTarget = HscNothing,
      ghcLink = NoLink,
      log_action = logHandler ref
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
      return $ "Failed at stage: loading\n" ++ show se
    Right sf -> do
      modSum <- getModSummary mn
      eitherp <- gtry (parseModule modSum) :: Ghc (Either SourceError ParsedModule)
      case eitherp of
        Left se -> do
          return $ "Failed at stage: parsing\n" ++ show se
        Right p -> do
          t <- gtry (typecheckModule p) :: Ghc (Either SourceError TypecheckedModule)
          case t of
            Left se -> do
              let j = unlines $ show <$> bagToList (srcErrorMessages se)
              return $ "Failed at stage: type checking\n" ++ show se
            Right tc -> do
              return "Program looks good"