{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ghc_lib_parser (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [9,4,1,20220807] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/lethe/.cabal/store/ghc-9.5.20220715/ghc-lib-parser-9.4.1.20220807-4701bf86012ad9d50259c0a1fe3f1f342f3a4a612188239805f6d567b3f49522/bin"
libdir     = "/home/lethe/.cabal/store/ghc-9.5.20220715/ghc-lib-parser-9.4.1.20220807-4701bf86012ad9d50259c0a1fe3f1f342f3a4a612188239805f6d567b3f49522/lib"
dynlibdir  = "/home/lethe/.cabal/store/ghc-9.5.20220715/ghc-lib-parser-9.4.1.20220807-4701bf86012ad9d50259c0a1fe3f1f342f3a4a612188239805f6d567b3f49522/lib"
datadir    = "/home/lethe/.cabal/store/ghc-9.5.20220715/ghc-lib-parser-9.4.1.20220807-4701bf86012ad9d50259c0a1fe3f1f342f3a4a612188239805f6d567b3f49522/share"
libexecdir = "/home/lethe/.cabal/store/ghc-9.5.20220715/ghc-lib-parser-9.4.1.20220807-4701bf86012ad9d50259c0a1fe3f1f342f3a4a612188239805f6d567b3f49522/libexec"
sysconfdir = "/home/lethe/.cabal/store/ghc-9.5.20220715/ghc-lib-parser-9.4.1.20220807-4701bf86012ad9d50259c0a1fe3f1f342f3a4a612188239805f6d567b3f49522/etc"

getBinDir     = catchIO (getEnv "ghc_lib_parser_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ghc_lib_parser_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ghc_lib_parser_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ghc_lib_parser_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ghc_lib_parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ghc_lib_parser_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
