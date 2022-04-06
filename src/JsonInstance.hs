module JsonInstance where

import Run
import Kanren
import Reasoning
import Data.Aeson
import Language.Haskell.Exts.SrcLoc

import Wrasse

instance ToJSON TypeForm
instance ToJSON SrcLoc
instance ToJSON Order
instance ToJSON ChStep
instance ToJSON SrcSpan
instance ToJSON ChContext
instance ToJSON ChResult
instance ToJSON Affinity

instance ToJSON GHCResult