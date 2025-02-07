{-# LINE 1 "libraries/ghc-heap/GHC/Exts/Heap/InfoTable/Types.hsc" #-}
{-# LANGUAGE DeriveGeneric #-}
module GHC.Exts.Heap.InfoTable.Types
    ( StgInfoTable(..)
    , EntryFunPtr
    , HalfWord
    , ItblCodes
    ) where



import Prelude -- See note [Why do we import Prelude here?]
import GHC.Generics
import GHC.Exts.Heap.ClosureTypes
import Foreign

type ItblCodes = Either [Word8] [Word32]


-- Ultra-minimalist version specially for constructors

{-# LINE 21 "libraries/ghc-heap/GHC/Exts/Heap/InfoTable/Types.hsc" #-}
type HalfWord = Word32

{-# LINE 27 "libraries/ghc-heap/GHC/Exts/Heap/InfoTable/Types.hsc" #-}

type EntryFunPtr = FunPtr (Ptr () -> IO (Ptr ()))

-- | This is a somewhat faithful representation of an info table. See
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/InfoTables.h>
-- for more details on this data structure.
data StgInfoTable = StgInfoTable {
   entry  :: Maybe EntryFunPtr, -- Just <=> not TABLES_NEXT_TO_CODE
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: ClosureType,
   srtlen :: HalfWord,
   code   :: Maybe ItblCodes -- Just <=> TABLES_NEXT_TO_CODE
  } deriving (Show, Generic)
