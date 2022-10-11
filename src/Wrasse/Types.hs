{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Wrasse.Types where

-- import external types
import Data.Tree ( Tree ) 
import GHC.Generics ( Generic ) 


-- | creates a default / empty instance. Normally used for padding
class Default a where
    def :: a

-- type for info about a tool as (Name, [(key, [lines])])
type ToolInfo = (String, [(String, [String])])

-- a result from /ghc endpoint
data WrasseResult
  = WrasseResult {
    raw :: [ToolInfo],
    ghc :: GHCResult,
    types :: String,
    fixes :: String,
    full :: Tree (String, Bool, Int),
    t :: [String]
  }
  deriving (Show, Generic)

-- a result from a GHC pass
data GHCResult
  = GHCResult
      {
        console :: [String],
        code :: [String]
      }       
      deriving (Show, Generic)

-- an example from HEMI database
data GHCExample
  = GHCExample
      {
        exTitle :: String
      , errorMsg :: [String]
      , explanation :: String
      , beforeCode :: [String]
      , afterCode :: [String]
      }
      deriving (Show, Generic)

-- a HEMI database error message
data GHCMessage
  = GHCMessage
      {
        errCode :: String
      , title :: String
      , summary :: String
      , severity :: String
      , introduced :: String
      , removed :: String
      , extension :: String
      , flag :: String
      , bodyText :: [String]
      , examples :: [GHCExample]
      }       
      deriving (Show, Generic)

-- a result from GHCI about a term
data GHCIInfo
  = GHCIInfo
    {
      definition :: String
    , symbolType :: String
    , symbolName :: String
    , symbolDefinedAt :: [String]
    , symbolEtc :: String
    }
    deriving (Show, Generic)
