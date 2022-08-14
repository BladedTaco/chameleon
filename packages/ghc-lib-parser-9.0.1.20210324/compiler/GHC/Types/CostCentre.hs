{-# LANGUAGE DeriveDataTypeable #-}
module GHC.Types.CostCentre (
        CostCentre(..), CcName, CCFlavour(..),
                -- All abstract except to friend: ParseIface.y

        CostCentreStack,
        CollectedCCs, emptyCollectedCCs, collectCC,
        currentCCS, dontCareCCS,
        isCurrentCCS,
        maybeSingletonCCS,

        mkUserCC, mkAutoCC, mkAllCafsCC,
        mkSingletonCCS,
        isCafCCS, isCafCC, isSccCountCC, sccAbleCC, ccFromThisModule,

        pprCostCentreCore,
        costCentreUserName, costCentreUserNameFS,
        costCentreSrcSpan,

        cmpCostCentre   -- used for removing dups in a list
    ) where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Types.Var
import GHC.Types.Name
import GHC.Unit.Module
import GHC.Types.Unique
import GHC.Utils.Outputable
import GHC.Types.SrcLoc
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Types.CostCentre.State

import Data.Data

-----------------------------------------------------------------------------
-- Cost Centres

-- | A Cost Centre is a single @{-# SCC #-}@ annotation.

data CostCentre
  = NormalCC {
                cc_flavour  :: CCFlavour,
                 -- ^ Two cost centres may have the same name and
                 -- module but different SrcSpans, so we need a way to
                 -- distinguish them easily and give them different
                 -- object-code labels.  So every CostCentre has an
                 -- associated flavour that indicates how it was
                 -- generated, and flavours that allow multiple instances
                 -- of the same name and module have a deterministic 0-based
                 -- index.
                cc_name :: CcName,      -- ^ Name of the cost centre itself
                cc_mod  :: Module,      -- ^ Name of module defining this CC.
                cc_loc  :: SrcSpan
    }

  | AllCafsCC {
                cc_mod  :: Module,      -- Name of module defining this CC.
                cc_loc  :: SrcSpan
    }
  deriving Data

type CcName = FastString

-- | The flavour of a cost centre.
--
-- Index fields represent 0-based indices giving source-code ordering of
-- centres with the same module, name, and flavour.
data CCFlavour = CafCC -- ^ Auto-generated top-level thunk
               | ExprCC !CostCentreIndex -- ^ Explicitly annotated expression
               | DeclCC !CostCentreIndex -- ^ Explicitly annotated declaration
               | HpcCC !CostCentreIndex -- ^ Generated by HPC for coverage
               deriving (Eq, Ord, Data)

-- | Extract the index from a flavour
flavourIndex :: CCFlavour -> Int
flavourIndex CafCC = 0
flavourIndex (ExprCC x) = unCostCentreIndex x
flavourIndex (DeclCC x) = unCostCentreIndex x
flavourIndex (HpcCC x) = unCostCentreIndex x

instance Eq CostCentre where
        c1 == c2 = case c1 `cmpCostCentre` c2 of { EQ -> True; _ -> False }

instance Ord CostCentre where
        compare = cmpCostCentre

cmpCostCentre :: CostCentre -> CostCentre -> Ordering

cmpCostCentre (AllCafsCC  {cc_mod = m1}) (AllCafsCC  {cc_mod = m2})
  = m1 `compare` m2

cmpCostCentre NormalCC {cc_flavour = f1, cc_mod =  m1, cc_name = n1}
              NormalCC {cc_flavour = f2, cc_mod =  m2, cc_name = n2}
    -- first key is module name, then centre name, then flavour
  = (m1 `compare` m2) `thenCmp` (n1 `compare` n2) `thenCmp` (f1 `compare` f2)

cmpCostCentre other_1 other_2
  = let
        tag1 = tag_CC other_1
        tag2 = tag_CC other_2
    in
    if tag1 < tag2 then LT else GT
  where
    tag_CC :: CostCentre -> Int
    tag_CC (NormalCC   {}) = 0
    tag_CC (AllCafsCC  {}) = 1


-----------------------------------------------------------------------------
-- Predicates on CostCentre

isCafCC :: CostCentre -> Bool
isCafCC (AllCafsCC {})                  = True
isCafCC (NormalCC {cc_flavour = CafCC}) = True
isCafCC _                               = False

-- | Is this a cost-centre which records scc counts
isSccCountCC :: CostCentre -> Bool
isSccCountCC cc | isCafCC cc  = False
                | otherwise   = True

-- | Is this a cost-centre which can be sccd ?
sccAbleCC :: CostCentre -> Bool
sccAbleCC cc | isCafCC cc = False
             | otherwise  = True

ccFromThisModule :: CostCentre -> Module -> Bool
ccFromThisModule cc m = cc_mod cc == m


-----------------------------------------------------------------------------
-- Building cost centres

mkUserCC :: FastString -> Module -> SrcSpan -> CCFlavour -> CostCentre
mkUserCC cc_name mod loc flavour
  = NormalCC { cc_name = cc_name, cc_mod =  mod, cc_loc = loc,
               cc_flavour = flavour
    }

mkAutoCC :: Id -> Module -> CostCentre
mkAutoCC id mod
  = NormalCC { cc_name = str, cc_mod =  mod,
               cc_loc = nameSrcSpan (getName id),
               cc_flavour = CafCC
    }
  where
        name = getName id
        -- beware: only external names are guaranteed to have unique
        -- Occnames.  If the name is not external, we must append its
        -- Unique.
        -- See bug #249, tests prof001, prof002,  also #2411
        str | isExternalName name = occNameFS (getOccName id)
            | otherwise           = occNameFS (getOccName id)
                                    `appendFS`
                                    mkFastString ('_' : show (getUnique name))
mkAllCafsCC :: Module -> SrcSpan -> CostCentre
mkAllCafsCC m loc = AllCafsCC { cc_mod = m, cc_loc = loc }

-----------------------------------------------------------------------------
-- Cost Centre Stacks

-- | A Cost Centre Stack is something that can be attached to a closure.
-- This is either:
--
--      * the current cost centre stack (CCCS)
--      * a pre-defined cost centre stack (there are several
--        pre-defined CCSs, see below).

data CostCentreStack
  = CurrentCCS          -- Pinned on a let(rec)-bound
                        -- thunk/function/constructor, this says that the
                        -- cost centre to be attached to the object, when it
                        -- is allocated, is whatever is in the
                        -- current-cost-centre-stack register.

  | DontCareCCS         -- We need a CCS to stick in static closures
                        -- (for data), but we *don't* expect them to
                        -- accumulate any costs.  But we still need
                        -- the placeholder.  This CCS is it.

  | SingletonCCS CostCentre

  deriving (Eq, Ord)    -- needed for Ord on CLabel


-- synonym for triple which describes the cost centre info in the generated
-- code for a module.
type CollectedCCs
  = ( [CostCentre]       -- local cost-centres that need to be decl'd
    , [CostCentreStack]  -- pre-defined "singleton" cost centre stacks
    )

emptyCollectedCCs :: CollectedCCs
emptyCollectedCCs = ([], [])

collectCC :: CostCentre -> CostCentreStack -> CollectedCCs -> CollectedCCs
collectCC cc ccs (c, cs) = (cc : c, ccs : cs)

currentCCS, dontCareCCS :: CostCentreStack

currentCCS              = CurrentCCS
dontCareCCS             = DontCareCCS

-----------------------------------------------------------------------------
-- Predicates on Cost-Centre Stacks

isCurrentCCS :: CostCentreStack -> Bool
isCurrentCCS CurrentCCS                 = True
isCurrentCCS _                          = False

isCafCCS :: CostCentreStack -> Bool
isCafCCS (SingletonCCS cc)              = isCafCC cc
isCafCCS _                              = False

maybeSingletonCCS :: CostCentreStack -> Maybe CostCentre
maybeSingletonCCS (SingletonCCS cc)     = Just cc
maybeSingletonCCS _                     = Nothing

mkSingletonCCS :: CostCentre -> CostCentreStack
mkSingletonCCS cc = SingletonCCS cc


-----------------------------------------------------------------------------
-- Printing Cost Centre Stacks.

-- The outputable instance for CostCentreStack prints the CCS as a C
-- expression.

instance Outputable CostCentreStack where
  ppr CurrentCCS        = text "CCCS"
  ppr DontCareCCS       = text "CCS_DONT_CARE"
  ppr (SingletonCCS cc) = ppr cc <> text "_ccs"


-----------------------------------------------------------------------------
-- Printing Cost Centres
--
-- There are several different ways in which we might want to print a
-- cost centre:
--
--      - the name of the cost centre, for profiling output (a C string)
--      - the label, i.e. C label for cost centre in .hc file.
--      - the debugging name, for output in -ddump things
--      - the interface name, for printing in _scc_ exprs in iface files.
--
-- The last 3 are derived from costCentreStr below.  The first is given
-- by costCentreName.

instance Outputable CostCentre where
  ppr cc = getPprStyle $ \ sty ->
           if codeStyle sty
           then ppCostCentreLbl cc
           else text (costCentreUserName cc)

-- Printing in Core
pprCostCentreCore :: CostCentre -> SDoc
pprCostCentreCore (AllCafsCC {cc_mod = m})
  = text "__sccC" <+> braces (ppr m)
pprCostCentreCore (NormalCC {cc_flavour = flavour, cc_name = n,
                             cc_mod = m, cc_loc = loc})
  = text "__scc" <+> braces (hsep [
        ppr m <> char '.' <> ftext n,
        pprFlavourCore flavour,
        whenPprDebug (ppr loc)
    ])

-- ^ Print a flavour in Core
pprFlavourCore :: CCFlavour -> SDoc
pprFlavourCore CafCC = text "__C"
pprFlavourCore f     = pprIdxCore $ flavourIndex f

-- ^ Print a flavour's index in Core
pprIdxCore :: Int -> SDoc
pprIdxCore 0 = empty
pprIdxCore idx = whenPprDebug $ ppr idx

-- Printing as a C label
ppCostCentreLbl :: CostCentre -> SDoc
ppCostCentreLbl (AllCafsCC  {cc_mod = m}) = ppr m <> text "_CAFs_cc"
ppCostCentreLbl (NormalCC {cc_flavour = f, cc_name = n, cc_mod = m})
  = ppr m <> char '_' <> ztext (zEncodeFS n) <> char '_' <>
        ppFlavourLblComponent f <> text "_cc"

-- ^ Print the flavour component of a C label
ppFlavourLblComponent :: CCFlavour -> SDoc
ppFlavourLblComponent CafCC = text "CAF"
ppFlavourLblComponent (ExprCC i) = text "EXPR" <> ppIdxLblComponent i
ppFlavourLblComponent (DeclCC i) = text "DECL" <> ppIdxLblComponent i
ppFlavourLblComponent (HpcCC i) = text "HPC" <> ppIdxLblComponent i

-- ^ Print the flavour index component of a C label
ppIdxLblComponent :: CostCentreIndex -> SDoc
ppIdxLblComponent n =
  case unCostCentreIndex n of
    0 -> empty
    n -> ppr n

-- This is the name to go in the user-displayed string,
-- recorded in the cost centre declaration
costCentreUserName :: CostCentre -> String
costCentreUserName = unpackFS . costCentreUserNameFS

costCentreUserNameFS :: CostCentre -> FastString
costCentreUserNameFS (AllCafsCC {})  = mkFastString "CAF"
costCentreUserNameFS (NormalCC {cc_name = name, cc_flavour = is_caf})
  =  case is_caf of
      CafCC -> mkFastString "CAF:" `appendFS` name
      _     -> name

costCentreSrcSpan :: CostCentre -> SrcSpan
costCentreSrcSpan = cc_loc

instance Binary CCFlavour where
    put_ bh CafCC = do
            putByte bh 0
    put_ bh (ExprCC i) = do
            putByte bh 1
            put_ bh i
    put_ bh (DeclCC i) = do
            putByte bh 2
            put_ bh i
    put_ bh (HpcCC i) = do
            putByte bh 3
            put_ bh i
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return CafCC
              1 -> ExprCC <$> get bh
              2 -> DeclCC <$> get bh
              _ -> HpcCC <$> get bh

instance Binary CostCentre where
    put_ bh (NormalCC aa ab ac _ad) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
            put_ bh ac
    put_ bh (AllCafsCC ae _af) = do
            putByte bh 1
            put_ bh ae
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      ab <- get bh
                      ac <- get bh
                      return (NormalCC aa ab ac noSrcSpan)
              _ -> do ae <- get bh
                      return (AllCafsCC ae noSrcSpan)

    -- We ignore the SrcSpans in CostCentres when we serialise them,
    -- and set the SrcSpans to noSrcSpan when deserialising.  This is
    -- ok, because we only need the SrcSpan when declaring the
    -- CostCentre in the original module, it is not used by importing
    -- modules.
