{-# OPTIONS_GHC -optc-DPROFILING #-}
{-# LINE 1 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module GHC.Exts.Heap.FFIClosures_ProfilingEnabled where

-- See [hsc and CPP workaround]




import Prelude
import Foreign
import GHC.Exts
import GHC.Exts.Heap.ProfInfo.PeekProfInfo
import GHC.Exts.Heap.ProfInfo.Types
import GHC.Exts.Heap.Closures(WhatNext(..), WhyBlocked(..), TsoFlags(..))

data TSOFields = TSOFields {
    tso_what_next :: WhatNext,
    tso_why_blocked :: WhyBlocked,
    tso_flags :: [TsoFlags],
-- Unfortunately block_info is a union without clear discriminator.
--    block_info :: TDB,
    tso_threadId :: Word64,
    tso_saved_errno :: Word32,
    tso_dirty:: Word32,
    tso_alloc_limit :: Int64,
    tso_tot_stack_size :: Word32,
    tso_prof :: Maybe StgTSOProfInfo
}

-- | Get non-pointer fields from @StgTSO_@ (@TSO.h@)
peekTSOFields :: (Ptr a -> IO (Maybe CostCentreStack)) -> Ptr tsoPtr -> IO TSOFields
peekTSOFields decodeCCS ptr = do
    what_next' <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) ptr
{-# LINE 36 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    why_blocked' <- ((\hsc_ptr -> peekByteOff hsc_ptr 50)) ptr
{-# LINE 37 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    flags' <- ((\hsc_ptr -> peekByteOff hsc_ptr 52)) ptr
{-# LINE 38 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    threadId' <- ((\hsc_ptr -> peekByteOff hsc_ptr 64)) ptr
{-# LINE 39 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    saved_errno' <- ((\hsc_ptr -> peekByteOff hsc_ptr 72)) ptr
{-# LINE 40 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    dirty' <- ((\hsc_ptr -> peekByteOff hsc_ptr 76)) ptr
{-# LINE 41 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    alloc_limit' <- ((\hsc_ptr -> peekByteOff hsc_ptr 120)) ptr
{-# LINE 42 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    tot_stack_size' <- ((\hsc_ptr -> peekByteOff hsc_ptr 128)) ptr
{-# LINE 43 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    tso_prof' <- peekStgTSOProfInfo decodeCCS ptr

    return TSOFields {
        tso_what_next = parseWhatNext what_next',
        tso_why_blocked = parseWhyBlocked why_blocked',
        tso_flags = parseTsoFlags flags',
        tso_threadId = threadId',
        tso_saved_errno = saved_errno',
        tso_dirty = dirty',
        tso_alloc_limit = alloc_limit',
        tso_tot_stack_size = tot_stack_size',
        tso_prof = tso_prof'
    }

parseWhatNext :: Word16 -> WhatNext
parseWhatNext w = case w of
                    (1) -> ThreadRunGHC
{-# LINE 60 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                    (2) -> ThreadInterpret
{-# LINE 61 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                    (3) -> ThreadKilled
{-# LINE 62 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                    (4) -> ThreadComplete
{-# LINE 63 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                    _ -> WhatNextUnknownValue w

parseWhyBlocked :: Word16 -> WhyBlocked
parseWhyBlocked w = case w of
                        (0) -> NotBlocked
{-# LINE 68 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (1) -> BlockedOnMVar
{-# LINE 69 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (14) -> BlockedOnMVarRead
{-# LINE 70 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (2) -> BlockedOnBlackHole
{-# LINE 71 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (3) -> BlockedOnRead
{-# LINE 72 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (4) -> BlockedOnWrite
{-# LINE 73 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (5) -> BlockedOnDelay
{-# LINE 74 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (6) -> BlockedOnSTM
{-# LINE 75 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (7) -> BlockedOnDoProc
{-# LINE 76 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (10) -> BlockedOnCCall
{-# LINE 77 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (11) -> BlockedOnCCall_Interruptible
{-# LINE 78 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (12) -> BlockedOnMsgThrowTo
{-# LINE 79 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        (13) -> ThreadMigrating
{-# LINE 80 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                        _ -> WhyBlockedUnknownValue w

parseTsoFlags :: Word32 -> [TsoFlags]
parseTsoFlags w | isSet (2) w = TsoLocked : parseTsoFlags (unset (2) w)
{-# LINE 84 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                | isSet (4) w = TsoBlockx : parseTsoFlags (unset (4) w)
{-# LINE 85 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                | isSet (8) w = TsoInterruptible : parseTsoFlags (unset (8) w)
{-# LINE 86 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                | isSet (16) w = TsoStoppedOnBreakpoint : parseTsoFlags (unset (16) w)
{-# LINE 87 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                | isSet (64) w = TsoMarked : parseTsoFlags (unset (64) w)
{-# LINE 88 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                | isSet (128) w = TsoSqueezed : parseTsoFlags (unset (128) w)
{-# LINE 89 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
                | isSet (256) w = TsoAllocLimit : parseTsoFlags (unset (256) w)
{-# LINE 90 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
parseTsoFlags 0 = []
parseTsoFlags w = [TsoFlagsUnknownValue w]

isSet :: Word32 -> Word32 -> Bool
isSet bitMask w = w .&. bitMask /= 0

unset :: Word32 -> Word32 -> Word32
unset bitMask w = w `xor` bitMask

data StackFields = StackFields {
    stack_size :: Word32,
    stack_dirty :: Word8,

{-# LINE 103 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    stack_marking :: Word8,

{-# LINE 105 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    stack_sp :: Addr#
}

-- | Get non-closure fields from @StgStack_@ (@TSO.h@)
peekStackFields :: Ptr a -> IO StackFields
peekStackFields ptr = do
    stack_size' <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr ::IO Word32
{-# LINE 112 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    dirty' <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 113 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}

{-# LINE 114 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    marking' <- ((\hsc_ptr -> peekByteOff hsc_ptr 29)) ptr
{-# LINE 115 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}

{-# LINE 116 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
    Ptr sp' <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 117 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}

    -- TODO decode the stack.

    return StackFields {
        stack_size = stack_size',
        stack_dirty = dirty',

{-# LINE 124 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
        stack_marking = marking',

{-# LINE 126 "libraries/ghc-heap/GHC/Exts/Heap/FFIClosures_ProfilingEnabled.hsc" #-}
        stack_sp = sp'
    }
