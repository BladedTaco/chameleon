{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveGeneric #-}

module Wrasse where

import Exception
import GHC
import GHC.Paths
import HscTypes
import System.Environment


import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import GHC.Generics
import Text.Regex.Applicative

data GHCResult
  = GHCResult
      { payload :: String
      }
      deriving (Show, Generic)

main :: IO ()
main = do
  args <- getArgs :: IO [String]
  r <- runGhc (Just libdir) (process (head args))
  putStrLn r

ghcFile :: FilePath
ghcFile = "generated/inFile.hs"

-- the entrypoint
hook :: String -> IO GHCResult
hook f = do
  createDirectoryIfMissing True $ takeDirectory ghcFile
  writeFile ghcFile f
  result <- runGhc (Just libdir) (process ghcFile)
  return $ GHCResult result
  -- return $ GHCResult ""

moduleParser :: RE Char String
moduleParser = do
    string "module "
    res <- many $ noneof " "
    string " where"
    return res

getModuleName :: String -> String
getModuleName s = moduleParser s

-- the boilerplate GHC
process :: FilePath -> Ghc String
process path = do
  dflags <- getSessionDynFlags
  let dflags' = dflags {hscTarget = HscNothing, ghcLink = NoLink}
  setSessionDynFlags dflags'
  let mn = mkModuleName "Task1"
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