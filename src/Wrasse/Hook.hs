{-# LANGUAGE ScopedTypeVariables, PackageImports  #-}
{-# LANGUAGE TupleSections #-}

module Wrasse.Hook (hook) where

import "ghc" Exception ( gtry )

import "ghc" GhcMonad ( liftIO )

import "ghc" HscTypes ( SourceError )

import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.FilePath.Posix (takeDirectory)

import Text.Parsec ( anyChar, noneOf, string, manyTill, many, parse, Parsec )
import "ghc" Bag ( bagToList )
import "ghc" Outputable ( SDoc(runSDoc), initSDocContext, Outputable (ppr), showSDocUnsafe )

import "ghc" DynFlags ( LogAction, dopt_set, gopt_set, DumpFlag(Opt_D_dump_json) )
import "ghc" ErrUtils ( mkLocMessage )

import GHC.IORef (newIORef, IORef)
import Data.IORef ( IORef, modifyIORef', newIORef, readIORef )
import HieAst (mkHieFile)

import Language.Haskell.HLint ( defaultParseFlags, parseModuleEx, hlint, ModuleEx, ParseError )

import Wrasse.Types
import Data.Tree (Tree(..), drawTree)
import Data.List (intercalate, isInfixOf)

import Wrasse.Tree (multiLevel)
import "ghc" Util (uncurry3, OverridingBool (Always))
import Control.Lens (traverseOf, Each (each), (^..))
import Agda.Utils.Tuple (uncurry4)
import DriverPipeline (preprocess)
import System.Process
    ( readCreateProcessWithExitCode,
      shell,
      CreateProcess(new_session, create_group) )

import Text.Regex ( matchRegex, mkRegex )
import Data.List.Split ( splitOn )
import Control.Monad (ap)
import Data.Aeson (encode)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (fromMaybe)
import Text.Parsec.Combinator (manyTill)

import Wrasse.Util
import JsonInstance
import GHC

-- map a module name to filepath for source code
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
  return $ WrasseResult tools ghcData "" "" ((, False, -1) <$> multiLevel tools) []


-- The hook for the various tools
toolHook :: String -> FilePath -> IO [(String, [(String, [String])])]
toolHook modName file = do
  (GHCResult g1 g2) <- ghcHook modName file
  (h1, h2) <- hlintHook file
  (d1, d2, d3) <- ghcAltHook modName file
  return [
    ("GHC", [
      ("output", g1),
      ("code", g2)
    ]),
    ("Defer GHC", [
      ("symbols", d1),
      ("output", d2),
      ("code", d3)
    ]),
    ("HLint", [
      ("console", h1),
      ("output", h2)
    ])
    ]


-- handles Defer GHC subtree
ghcAltHook :: String -> FilePath -> IO ([String], [String], [String])
ghcAltHook modName file = do
  let flags = [
          "-fprint-potential-instances"
        , "-ferror-spans"
        , "-Wall"
        , "-fdefer-type-errors"
        , "--interactive"
        ]

  -- get list of symbols from ghci
  let cmd = "ghc " ++ unwords flags ++ " generated/Infile.hs"
  let sIn = ":browse! *" ++ modName

  (_, sOut, sErr) <- readCreateProcessWithExitCode ((shell cmd) {new_session = True, create_group = True}) sIn

  -- parse string to get symbols names
  let reg = mkRegex "^([^:= ]* )? *([^:=]+)(( :: )|( => )|( = ))"
  let symbols = filter (isInfixOf " :: " <||> isInfixOf " => " <||> isInfixOf " = ") $ lines sOut
  let symbolNames = filter (/= ["", "", ""]) $ map (ap (:) $ trim <$.> take 2 . fromMaybe ["", ""] . matchRegex reg) symbols

  -- poll ghci again to get symbol information
  let sIn2 = intercalate "\n" $ map ((":info " ++) . last) symbolNames
  (_, sOut2, sErr2) <- readCreateProcessWithExitCode ((shell cmd) {new_session = True, create_group = True}) sIn2

  -- parse text and join symbol info with symbol name
  let symbols = padZipGeneral ["", "", ""] "" symbolNames $ tail $ splitOn "ghci> " sOut2
  let defReg = mkRegex "Defined (at|in) ([^ ]*)"
  let definedAt = fromMaybe ["", ""] . matchRegex defReg
  -- output into GHCIInfo list
  let sym = (\(x1, x4) -> GHCIInfo (head x1) (x1!!1) (x1!!2) (definedAt x4) x4) <$> symbols

  -- read the code into a string
  fileContents <- readFile file

  -- return information
  return (toString . encode <$> sym, lines $ sErr ++ sErr2, lines fileContents)


-- runs GHC on the given code file
ghcHook :: String -> FilePath -> IO GHCResult
ghcHook modName file = do
  let flags = [
          "-fprint-potential-instances"
        , "-fforce-recomp"
        , "-fno-code"
        , "-fobject-code"
        , "-fwarn-monomorphism-restriction"
        , "-fwarn-name-shadowing "
        , "-Wmissed-specialisations"
        , "-ferror-spans"
        , "-Wall"
        , "-fdefer-diagnostics"
        , "-fprint-axiom-incomps"
        , "-fprint-equality-relations"
        , "-fprint-expanded-synonyms"
        , "-fprint-explicit-coercions"
        , "-fprint-typechecker-elaboration"
        , "-fhelpful-errors"
        , "-fshow-warning-groups"
        ]

  let cmd = "ghc " ++ unwords flags ++ " generated/Infile.hs"
  let sIn = ""
  (_, sOut, sErr) <- readCreateProcessWithExitCode ((shell cmd) {new_session = True, create_group = True}) sIn

  -- read the code into a string
  fileContents <- readFile file

  -- return result
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
  return (lines ref_out, out ++ [""] ++ (show <$> ideas))

-- parser for the input code to extract the module name
moduleParser :: Parsec String () String
moduleParser = do
    manyTill anyChar (string "module ")
    res <- many $ noneOf " "
    string " where"
    return res

-- gets the module name from the code
getModuleName :: String -> String
getModuleName s = case parse moduleParser "" s of
    Left err -> ""
    Right xs -> xs

-- deprecated, loghandler for runghc function to capture output
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



-- the boilerplate Hlint
processHLint :: Either Language.Haskell.HLint.ParseError ModuleEx -> [String]
processHLint e = do
  case e of
    Left pe -> ["Failed compiling"]
    Right me -> ["COMPILED SUCCESSFULLY"]



-- the boilerplate GHC (deprecated)
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

  -- add in dump and general flags to dynflags
  let dflags'' = foldl dopt_set dflags' dpflags
  let dflags''' = foldl gopt_set dflags'' gflags

  -- setup session
  setSessionDynFlags dflags'''
  let mn = mkModuleName moduleName
  let hsTarketId = TargetFile path Nothing
  addTarget
    Target
      { targetId = hsTarketId,
        targetAllowObjCode = False,
        targetContents = Nothing
      }

  -- run session
  session <- getSession
  a <- liftIO $ preprocess session path Nothing Nothing
  let b = (
          case a of
            Left bag -> show $ bagToList bag
            Right x0 -> ("succ " ++) $ show $ snd x0
          )

  -- find at what stage GHC fails
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
              return (b, show se, showSDocUnsafe $ ppr ps)
            Right tc -> do
              let TypecheckedModule _ (Just rs) ts modInfo (typeGlobalEnv, moduleDeets) = tc
              let hieFile = mkHieFile modSum typeGlobalEnv rs

              return ("Program looks good","", showSDocUnsafe $ ppr ps)
