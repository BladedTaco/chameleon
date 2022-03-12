module JsonInstance where

import Run
import Reasoning
import Data.Aeson
import Language.Haskell.Exts.SrcLoc

instance ToJSON Order
instance ToJSON ChStep
instance ToJSON SrcSpan
instance ToJSON ChContext 
instance ToJSON ChResult 
