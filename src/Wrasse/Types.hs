{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Wrasse.Types where

import Data.Tree (Tree (Node))
import GHC.Generics (Generic)


type ToolInfo = (String, [(String, [String])])

data WrasseResult
  = WrasseResult {
    raw :: [ToolInfo],
    ghc :: GHCResult,
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
        failStage :: [String],
        output :: [String],
        code :: [String]
      }       
      deriving (Show, Generic)