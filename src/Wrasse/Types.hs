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

data GHCMessage
  = GHCMessage
      {
        title :: String,
        summary :: String,
        severity :: String,
        introduced :: String,
        extension :: String,
        flag :: String,
        bodyText :: [String]
      }       
      deriving (Show, Generic)

      
    {-
    
    ---
title: Empty single quotes
summary: No character literal provided within single quotes.
severity: error
introduced: 9.6.1
extension: TemplateHaskell
---

Single quotes are used for character and literals including "new line" (`'\n'`), "carriage return" (`'\r'`), "horizontal tab" (`'\t'`), and "vertical tab" (`'\v'`). The complete list of character escapes is described in [the Haskell Report](https://www.haskell.org/onlinereport/lexemes.html) section 2.6, Character and String Literals. Character literals cannot be empty.

Another usage of single quotes is in `TemplateHaskell`; please refer to the [GHC documentation](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/template_haskell.html#syntax) for details.

## Example error text

```
 error: [GHC-11861]
    Parser error on `''`
    Character literals may not be empty
```

    -}