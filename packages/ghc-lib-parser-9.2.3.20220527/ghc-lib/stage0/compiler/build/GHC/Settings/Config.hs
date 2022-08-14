{-# LANGUAGE CPP #-}
module GHC.Settings.Config
  ( module GHC.Version
  , cBuildPlatformString
  , cHostPlatformString
  , cProjectName
  , cBooterVersion
  , cStage
  ) where

import GHC.Prelude

import GHC.Version

cBuildPlatformString :: String
cBuildPlatformString = "x86_64-apple-darwin"

cHostPlatformString :: String
cHostPlatformString = "x86_64-apple-darwin"

cProjectName          :: String
cProjectName          = "The Glorious Glasgow Haskell Compilation System"

cBooterVersion        :: String
cBooterVersion        = "8.10.4"

cStage                :: String
cStage                = show (1 :: Int)
