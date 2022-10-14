module Wrasse.Instance where
    
import Data.Map 
import Data.Tree
import Wrasse.Types

{-
Default instancing for various types
-}

instance Default Char where
    def = '\NUL'

instance Default Int where
    def = 0

instance Default Bool where
    def = False

instance (Default a, Default b) => Default (a,b) where
    def = (,) def def

instance (Default a, Default b, Default c) => Default (a,b,c) where
    def = (,,) def def def

instance Default [a] where
    def = []

instance Default (Map a b) where
    def = empty

instance Default (Maybe a) where
    def = Nothing

instance (Default a) =>  Default (Tree a) where
    def = Node def []

instance Default GHCMessage where
    def = GHCMessage def def def def def def def def def def

instance Default GHCResult where
    def = GHCResult def def

instance Default WrasseResult where
    def = WrasseResult def def def def def def

instance Default GHCIInfo where
    def = GHCIInfo def def def def def