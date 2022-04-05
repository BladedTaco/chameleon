{-# LANGUAGE ScopedTypeVariables #-}

module Wrasse where

import Exception
import GHC
import GHC.Paths
import HscTypes
import System.Environment

main :: IO ()
main = do
  args <- getArgs :: IO [String]
  r <- runGhc (Just libdir) (process (head args))
  putStrLn r

hook :: FilePath -> Ghc String
hook f = process f


process :: FilePath -> Ghc String
process path = do
  dflags <- getSessionDynFlags
  let dflags' = dflags {hscTarget = HscNothing, ghcLink = NoLink}
  setSessionDynFlags dflags'
  let mn = mkModuleName "Example"
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
      return "Failed at stage: loading"
    Right sf -> do
      modSum <- getModSummary mn
      eitherp <- gtry (parseModule modSum) :: Ghc (Either SourceError ParsedModule)
      case eitherp of
        Left se -> do
          return "Failed at stage: parsing"
        Right p -> do
          t <- gtry (typecheckModule p) :: Ghc (Either SourceError TypecheckedModule)
          case t of
            Left se -> do
              return "Failed at stage: type checking"
            Right tc -> do
              return "Program looks good"