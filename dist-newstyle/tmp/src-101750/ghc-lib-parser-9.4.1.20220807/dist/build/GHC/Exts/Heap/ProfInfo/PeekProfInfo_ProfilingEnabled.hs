{-# OPTIONS_GHC -optc-DPROFILING #-}
{-# LINE 1 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}

module GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled(
    peekStgTSOProfInfo
    , peekTopCCS
) where


{-# LINE 10 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}

-- See [hsc and CPP workaround]









import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Foreign
import           Foreign.C.String
import           GHC.Exts
import           GHC.Exts.Heap.ProfInfo.Types
import           Prelude

-- Use Int based containers for pointers (addresses) for better performance.
-- These will be queried a lot!
type AddressSet = IntSet
type AddressMap = IntMap

peekStgTSOProfInfo :: (Ptr b -> IO (Maybe CostCentreStack)) -> Ptr a -> IO (Maybe StgTSOProfInfo)
peekStgTSOProfInfo decodeCCS tsoPtr = do
    cccs_ptr <- peekByteOff tsoPtr cccsOffset
    cccs' <- decodeCCS cccs_ptr

    return $ Just StgTSOProfInfo {
        cccs = cccs'
    }

peekTopCCS :: Ptr b -> IO (Maybe CostCentreStack)
peekTopCCS cccs_ptr = do
  costCenterCacheRef <- newIORef IntMap.empty
  peekCostCentreStack IntSet.empty costCenterCacheRef cccs_ptr

cccsOffset :: Int
cccsOffset = (112) + ((24))
{-# LINE 53 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}

peekCostCentreStack
    :: AddressSet
    -> IORef (AddressMap CostCentre)
    -> Ptr costCentreStack
    -> IO (Maybe CostCentreStack)
peekCostCentreStack _ _ ptr | ptr == nullPtr = return Nothing
peekCostCentreStack loopBreakers _ ptr | IntSet.member (ptrToInt ptr) loopBreakers = return Nothing
peekCostCentreStack loopBreakers costCenterCacheRef ptr = do
        ccs_ccsID' <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 63 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_cc_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 64 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_cc' <- peekCostCentre costCenterCacheRef ccs_cc_ptr
        ccs_prevStack_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 66 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        let loopBreakers' = (IntSet.insert ptrAsInt loopBreakers)
        ccs_prevStack' <- peekCostCentreStack loopBreakers' costCenterCacheRef ccs_prevStack_ptr
        ccs_indexTable_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 69 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_indexTable' <- peekIndexTable loopBreakers' costCenterCacheRef ccs_indexTable_ptr
        ccs_root_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 71 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_root' <- peekCostCentreStack loopBreakers' costCenterCacheRef ccs_root_ptr
        ccs_depth' <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 73 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_scc_count' <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) ptr
{-# LINE 74 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_selected' <- ((\hsc_ptr -> peekByteOff hsc_ptr 56)) ptr
{-# LINE 75 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_time_ticks' <- ((\hsc_ptr -> peekByteOff hsc_ptr 64)) ptr
{-# LINE 76 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_mem_alloc' <- ((\hsc_ptr -> peekByteOff hsc_ptr 72)) ptr
{-# LINE 77 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_inherited_alloc' <- ((\hsc_ptr -> peekByteOff hsc_ptr 80)) ptr
{-# LINE 78 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        ccs_inherited_ticks' <- ((\hsc_ptr -> peekByteOff hsc_ptr 88)) ptr
{-# LINE 79 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}

        return $ Just CostCentreStack {
            ccs_ccsID = ccs_ccsID',
            ccs_cc = ccs_cc',
            ccs_prevStack = ccs_prevStack',
            ccs_indexTable = ccs_indexTable',
            ccs_root = ccs_root',
            ccs_depth = ccs_depth',
            ccs_scc_count = ccs_scc_count',
            ccs_selected = ccs_selected',
            ccs_time_ticks = ccs_time_ticks',
            ccs_mem_alloc = ccs_mem_alloc',
            ccs_inherited_alloc = ccs_inherited_alloc',
            ccs_inherited_ticks = ccs_inherited_ticks'
        }
    where
        ptrAsInt = ptrToInt ptr

peekCostCentre :: IORef (AddressMap CostCentre) -> Ptr costCentre -> IO CostCentre
peekCostCentre costCenterCacheRef ptr = do
    costCenterCache <- readIORef costCenterCacheRef
    case IntMap.lookup ptrAsInt costCenterCache of
        (Just a) -> return a
        Nothing -> do
                    cc_ccID' <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 104 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
                    cc_label_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 105 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
                    cc_label' <- peekCString cc_label_ptr
                    cc_module_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 107 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
                    cc_module' <- peekCString cc_module_ptr
                    cc_srcloc_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 109 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
                    cc_srcloc' <- do
                        if cc_srcloc_ptr == nullPtr then
                            return Nothing
                        else
                            fmap Just (peekCString cc_srcloc_ptr)
                    cc_mem_alloc' <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 115 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
                    cc_time_ticks' <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 116 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
                    cc_is_caf' <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) ptr
{-# LINE 117 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
                    cc_link_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 56)) ptr
{-# LINE 118 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
                    cc_link' <- if cc_link_ptr == nullPtr then
                        return Nothing
                    else
                        fmap Just (peekCostCentre costCenterCacheRef cc_link_ptr)

                    let result = CostCentre {
                        cc_ccID = cc_ccID',
                        cc_label = cc_label',
                        cc_module = cc_module',
                        cc_srcloc = cc_srcloc',
                        cc_mem_alloc = cc_mem_alloc',
                        cc_time_ticks = cc_time_ticks',
                        cc_is_caf = cc_is_caf',
                        cc_link = cc_link'
                    }

                    writeIORef costCenterCacheRef (IntMap.insert ptrAsInt result costCenterCache)

                    return result
    where
        ptrAsInt = ptrToInt ptr

peekIndexTable :: AddressSet -> IORef (AddressMap CostCentre) -> Ptr indexTable -> IO (Maybe IndexTable)
peekIndexTable _ _ ptr | ptr == nullPtr = return Nothing
peekIndexTable loopBreakers costCenterCacheRef ptr = do
        it_cc_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 144 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        it_cc' <- peekCostCentre costCenterCacheRef it_cc_ptr
        it_ccs_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 146 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        it_ccs' <- peekCostCentreStack loopBreakers costCenterCacheRef it_ccs_ptr
        it_next_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 148 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
        it_next' <- peekIndexTable loopBreakers costCenterCacheRef it_next_ptr
        it_back_edge' <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 150 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}

        return $ Just IndexTable {
            it_cc = it_cc',
            it_ccs = it_ccs',
            it_next = it_next',
            it_back_edge = it_back_edge'
        }

-- | casts a @Ptr@ to an @Int@
ptrToInt :: Ptr a -> Int
ptrToInt (Ptr a#) = I# (addr2Int# a#)


{-# LINE 174 "libraries/ghc-heap/GHC/Exts/Heap/ProfInfo/PeekProfInfo_ProfilingEnabled.hsc" #-}
