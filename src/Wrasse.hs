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
import Text.Parsec

data GHCResult
  = GHCResult
      { payload :: String
      }
      deriving (Show, Generic)

main :: IO ()
main = do
  args <- getArgs :: IO [String]
  r <- runGhc (Just libdir) (process "Example" (head args))
  putStrLn r

ghcFile :: String -> FilePath
ghcFile = ("generated/" ++) . (++ ".hs")

-- the entrypoint
hook :: String -> IO GHCResult
hook f = do
  let m = getModuleName f
  let (modName, file, s) = if m == ""
      then ("Infile", ghcFile "Infile", "module Infile where\n" ++ f)
      else (m, ghcFile "Infile", f)
  createDirectoryIfMissing True $ takeDirectory file
  writeFile file s
  result <- runGhc (Just libdir) (process modName file)
  return $ GHCResult result
  -- return $ GHCResult ""

moduleParser :: Parsec String () String
moduleParser = do
    string "module "
    res <- many $ noneOf " "
    string " where"
    return res

getModuleName :: String -> String
getModuleName s = case (parse moduleParser "" s) of
    Left err -> ""
    Right xs -> xs

-- the boilerplate GHC
process :: String -> FilePath -> Ghc String
process moduleName path = do
  dflags <- getSessionDynFlags
  let dflags' = dflags {hscTarget = HscNothing, ghcLink = NoLink}
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