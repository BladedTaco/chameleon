{-# OPTIONS_GHC -optc-DPROFILING #-}
{-# LINE 1 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
{-# LANGUAGE NoMonoLocalBinds #-}
module GHC.Exts.Heap.InfoTableProf
    ( module GHC.Exts.Heap.InfoTable.Types
    , itblSize
    , peekItbl
    , pokeItbl
    ) where

-- This file overrides InfoTable.hsc's implementation of peekItbl and pokeItbl.
-- Manually defining PROFILING gives the #peek and #poke macros an accurate
-- representation of StgInfoTable_ when hsc2hs runs.



import Prelude -- See note [Why do we import Prelude here?]
import GHC.Exts.Heap.InfoTable.Types

{-# LINE 21 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
import Foreign

-- | Read an InfoTable from the heap into a haskell type.
-- WARNING: This code assumes it is passed a pointer to a "standard" info
-- table. If tables_next_to_code is enabled, it will look 1 byte before the
-- start for the entry field.
peekItbl :: Ptr StgInfoTable -> IO StgInfoTable
peekItbl a0 = do

{-# LINE 33 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  let ptr = a0
      entry' = Nothing

{-# LINE 36 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  ptrs'   <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 37 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  nptrs'  <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 38 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  tipe'   <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 39 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  srtlen' <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) a0
{-# LINE 40 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  return StgInfoTable
    { entry  = entry'
    , ptrs   = ptrs'
    , nptrs  = nptrs'
    , tipe   = toEnum (fromIntegral (tipe' :: HalfWord))
    , srtlen = srtlen'
    , code   = Nothing
    }

pokeItbl :: Ptr StgInfoTable -> StgInfoTable -> IO ()
pokeItbl a0 itbl = do

{-# LINE 54 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) a0 (ptrs itbl)
{-# LINE 55 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) a0 (nptrs itbl)
{-# LINE 56 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) a0 (fromEnum (tipe itbl))
{-# LINE 57 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  ((\hsc_ptr -> pokeByteOff hsc_ptr 28)) a0 (srtlen itbl)
{-# LINE 58 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}

{-# LINE 59 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  let code_offset = a0 `plusPtr` ((32))
{-# LINE 60 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
  case code itbl of
    Nothing -> return ()
    Just (Left xs) -> pokeArray code_offset xs
    Just (Right xs) -> pokeArray code_offset xs

{-# LINE 65 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}

itblSize :: Int
itblSize = ((32))
{-# LINE 68 "libraries/ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc" #-}
