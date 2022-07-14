{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Wrasse.Types where

import Data.Tree (Tree (Node))
import GHC.Generics (Generic)


type ToolInfo = (String, [String], [String])

data WrasseResult
  = WrasseResult {
    raw :: [ToolInfo],
    ghc :: String,
    types :: String,
    fixes :: String,
    full :: Tree (String, Bool, Integer),
    t :: [String]
  }
  deriving (Show, Generic)


data GHCResult
  = GHCResult
      {
        console :: [String],
        payload :: [String]
      }       
      deriving (Show, Generic)