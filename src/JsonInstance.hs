module JsonInstance where

import Data.Aeson
import Kanren
import Language.Haskell.Exts.SrcLoc
import Reasoning
import Run

import Wrasse.Types

instance ToJSON TypeForm

instance ToJSON SrcLoc

instance ToJSON Order

instance ToJSON ChStep

instance ToJSON SrcSpan

instance ToJSON ChContext

instance ToJSON ChResult

instance ToJSON Affinity


instance ToJSON GHCResult
instance ToJSON WrasseResult
instance ToJSON GHCMessage
instance ToJSON GHCExample