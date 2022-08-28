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
import Data.List (intercalate, isInfixOf)

import Wrasse.Tree (multiLevel)
import "ghc" Util (uncurry3, OverridingBool (Always))
import Control.Lens (traverseOf, Each (each))
import Control.Arrow
import Agda.Utils.Tuple (uncurry4)
import DriverPipeline (preprocess)

import Wrasse.Util

import System.Process 

import Text.Regex
import Data.Maybe

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
  (GHCResult d1 d2) <- ghcAltHook modName file
  return [
    ("GHC", [ 
      ("output", g1),
      ("code", g2)
    ]),
    ("Defer GHC", [
      ("output", d1),
      ("code", d2)
    ]),
    ("HLint", [
      ("console", h1),
      ("output", h2)
    ])
    ]


-- runs GHC
ghcAltHook :: String -> FilePath -> IO GHCResult
ghcAltHook modName file = do
  -- ref <- liftIO $ newIORef "" -- make an output IO stream
  -- result <- runGhc (Just libdir) (processGHC ref modName file)
  -- ref_out <- readIORef ref -- read the output IO stream
  -- return $ GHCResult (("ghc console: " ++) <$> lines ref_out) (lines $ show result)


  let flags = [
          "-fprint-potential-instances"
        , "-ferror-spans"
        , "-Wall"
        , "-fdefer-type-errors"
        , "--interactive"
        ]

  let cmd = "ghc " ++ unwords flags ++ " generated/Infile.hs"
  -- let sIn = ":browse!\n:show bindings"
  let sIn = ":browse! *" ++ modName

  (_, sOut, sErr) <- readCreateProcessWithExitCode ((shell cmd) {new_session = True, create_group = True}) sIn

  -- let reg = mkRegex "^(.*? )?([a-zA-Z]+?)(( :: )|( => )|( = ))"
  let reg = mkRegex "^(.*? )?([^ ]+) :: "

  let symbols = filter (isInfixOf " :: " <||> isInfixOf " => " <||> isInfixOf " = ") $ lines sOut 
  let symbolNames = filter (/= "") $ map (last . fromMaybe ["", ""] . matchRegex reg) symbols

  let stdInNew = intercalate "\n" $ map (":info " ++) symbolNames

  (_, sOut2, sErr2) <- readCreateProcessWithExitCode ((shell cmd) {new_session = True, create_group = True}) stdInNew

  fileContents <- readFile file

  -- return $ uncurry GHCResult $ mapTup2 lines (intercalate "\n" (show <$> symbolNames) ++ sOut2 ++ "\n" ++ sErr, fileContents)
  return $ uncurry GHCResult $ mapTup2 lines (sOut2 ++ "\n" ++ sErr ++ sErr2, fileContents)



--   let flags = [
--           "-fprint-potential-instances"
--         , "-ferror-spans"
--         , "-Wall"
--         , "-fdefer-type-errors"
--         ]

--   let cmd = "ghc " ++ unwords flags ++ " generated/Infile.hs"
--   let sIn = ""

--   (_, sOut, sErr) <- readCreateProcessWithExitCode ((shell cmd) {new_session = True, create_group = True}) sIn

--   fileContents <- readFile file

--   return $ uncurry GHCResult $ mapTup2 lines (sOut ++ "\n" ++ sErr, fileContents)



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
  ideas <- hlint ["generated", "--quiet", "-s"]
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








-- the boilerplate GHC
processGHC :: IORef String -> String -> FilePath -> Ghc (String, String, String)
processGHC ref moduleName path = do
  dflags <- getSessionDynFlags
  -- ref <- liftIO $ newIORef ""
  let dflags' = dflags {
      hscTarget = HscNothing
    , ghcLink = NoLink
    , log_action = logHandler ref
    , maxValidHoleFits = Nothing
    , refLevelHoleFits = Nothing
    , maxRefHoleFits = Nothing
    , maxRelevantBinds = Nothing
    , useColor = Always
    , maxErrors = Just 10
    }
  -- general flags
  let gflags = [ 
          Opt_KeepGoing
        , Opt_DiagnosticsShowCaret
        , Opt_PrintPotentialInstances
        , Opt_PrintTypecheckerElaboration
        , Opt_HelpfulErrors
        , Opt_ErrorSpans
        , Opt_Haddock
        , Opt_DeferTypeErrors
        , Opt_DeferTypedHoles
        , Opt_DeferOutOfScopeVariables
        ]
  -- dump flags
  let dpflags = [
          Opt_D_dump_json
        ]

  let dflags'' = foldl dopt_set dflags' dpflags
  let dflags''' = foldl gopt_set dflags'' gflags

  setSessionDynFlags dflags'''
  let mn = mkModuleName moduleName
  let hsTarketId = TargetFile path Nothing
  addTarget
    Target
      { targetId = hsTarketId,
        targetAllowObjCode = False,
        targetContents = Nothing
      }

  
  -- ref <- liftIO $ newIORef "" -- make an output IO stream
  -- result <- runGhc (Just libdir) (processGHC ref modName file)
  -- ref_out <- readIORef ref -- read the output IO stream

  session <- getSession
  a <- liftIO $ preprocess session path Nothing Nothing
  let b = (
          case a of
            Left bag -> show $ bagToList bag
            Right x0 -> ("succ " ++) $ show $ snd x0
          )


  eitherl <- gtry (load LoadAllTargets) :: Ghc (Either SourceError SuccessFlag)
  case eitherl of
    Left se -> do
      removeTarget hsTarketId
      return ("Failed at stage: loading", show se, "")
    Right sf -> do
      modSum <- getModSummary mn
      eitherp <- gtry (parseModule modSum) :: Ghc (Either SourceError ParsedModule)
      case eitherp of
        Left se -> do
          return ("Failed at stage: parsing", show se, "")
        Right p -> do
          t <- gtry (typecheckModule p) :: Ghc (Either SourceError TypecheckedModule)
          let ParsedModule _ ps imprts anns = p
          case t of
            Left se -> do
            
              -- return ("Failed at stage: type checking", show $ bagToList $ srcErrorMessages se, showSDocUnsafe $ ppr ps)
              -- return ("Failed at stage: type checking", show se, showSDocUnsafe $ ppr ps)
              return (b, show se, showSDocUnsafe $ ppr ps)
              -- return ("Failed at stage: type checking", show se, codeFile)
            Right tc -> do
              let TypecheckedModule _ (Just rs) ts modInfo (typeGlobalEnv, moduleDeets) = tc
              let hieFile = mkHieFile modSum typeGlobalEnv rs
              
              return ("Program looks good","", showSDocUnsafe $ ppr ps)
              -- return ("Program looks good", intercalate "\n\n" [
              --   show rs
              --   , show ts
              --   , show modInfo
              --   , show typeGlobalEnv
              --   , show moduleDeets
              --   ], showSDocUnsafe $ ppr ps)

