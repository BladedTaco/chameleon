{-
(c) The AQUA Project, Glasgow University, 1994-1998


Wired-in knowledge about primitive types
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module defines TyCons that can't be expressed in Haskell.
--   They are all, therefore, wired-in TyCons.  C.f module "GHC.Builtin.Types"
module GHC.Builtin.Types.Prim(
        mkPrimTyConName, -- For implicit parameters in GHC.Builtin.Types only

        mkTemplateKindVars, mkTemplateTyVars, mkTemplateTyVarsFrom,
        mkTemplateKiTyVars, mkTemplateKiTyVar,

        mkTemplateTyConBinders, mkTemplateKindTyConBinders,
        mkTemplateAnonTyConBinders,

        alphaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
        alphaTys, alphaTy, betaTy, gammaTy, deltaTy,
        alphaTyVarsUnliftedRep, alphaTyVarUnliftedRep,
        alphaTysUnliftedRep, alphaTyUnliftedRep,
        runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep3TyVar,
        runtimeRep1Ty, runtimeRep2Ty, runtimeRep3Ty,

        openAlphaTyVar, openBetaTyVar, openGammaTyVar,
        openAlphaTy, openBetaTy, openGammaTy,

        multiplicityTyVar1, multiplicityTyVar2,

        -- Kind constructors...
        tYPETyCon, tYPETyConName,

        -- Kinds
        tYPE, primRepToRuntimeRep,

        functionWithMultiplicity,
        funTyCon, funTyConName,
        unexposedPrimTyCons, exposedPrimTyCons, primTyCons,

        charPrimTyCon,          charPrimTy, charPrimTyConName,
        intPrimTyCon,           intPrimTy, intPrimTyConName,
        wordPrimTyCon,          wordPrimTy, wordPrimTyConName,
        addrPrimTyCon,          addrPrimTy, addrPrimTyConName,
        floatPrimTyCon,         floatPrimTy, floatPrimTyConName,
        doublePrimTyCon,        doublePrimTy, doublePrimTyConName,

        statePrimTyCon,         mkStatePrimTy,
        realWorldTyCon,         realWorldTy, realWorldStatePrimTy,

        proxyPrimTyCon,         mkProxyPrimTy,

        arrayPrimTyCon, mkArrayPrimTy,
        byteArrayPrimTyCon,     byteArrayPrimTy,
        arrayArrayPrimTyCon, mkArrayArrayPrimTy,
        smallArrayPrimTyCon, mkSmallArrayPrimTy,
        mutableArrayPrimTyCon, mkMutableArrayPrimTy,
        mutableByteArrayPrimTyCon, mkMutableByteArrayPrimTy,
        mutableArrayArrayPrimTyCon, mkMutableArrayArrayPrimTy,
        smallMutableArrayPrimTyCon, mkSmallMutableArrayPrimTy,
        mutVarPrimTyCon, mkMutVarPrimTy,

        mVarPrimTyCon,                  mkMVarPrimTy,
        ioPortPrimTyCon,                mkIOPortPrimTy,
        tVarPrimTyCon,                  mkTVarPrimTy,
        stablePtrPrimTyCon,             mkStablePtrPrimTy,
        stableNamePrimTyCon,            mkStableNamePrimTy,
        compactPrimTyCon,               compactPrimTy,
        bcoPrimTyCon,                   bcoPrimTy,
        weakPrimTyCon,                  mkWeakPrimTy,
        threadIdPrimTyCon,              threadIdPrimTy,

        int8PrimTyCon,          int8PrimTy, int8PrimTyConName,
        word8PrimTyCon,         word8PrimTy, word8PrimTyConName,

        int16PrimTyCon,         int16PrimTy, int16PrimTyConName,
        word16PrimTyCon,        word16PrimTy, word16PrimTyConName,

        int32PrimTyCon,         int32PrimTy, int32PrimTyConName,
        word32PrimTyCon,        word32PrimTy, word32PrimTyConName,

        int64PrimTyCon,         int64PrimTy, int64PrimTyConName,
        word64PrimTyCon,        word64PrimTy, word64PrimTyConName,

        eqPrimTyCon,            -- ty1 ~# ty2
        eqReprPrimTyCon,        -- ty1 ~R# ty2  (at role Representational)
        eqPhantPrimTyCon,       -- ty1 ~P# ty2  (at role Phantom)
        equalityTyCon,

        -- * SIMD
#include "primop-vector-tys-exports.hs-incl"
  ) where

#include "GhclibHsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.Builtin.Types
  ( runtimeRepTy, unboxedTupleKind, liftedTypeKind
  , vecRepDataConTyCon, tupleRepDataConTyCon
  , liftedRepTy, unliftedRepTy
  , intRepDataConTy
  , int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy
  , wordRepDataConTy
  , word16RepDataConTy, word8RepDataConTy, word32RepDataConTy, word64RepDataConTy
  , addrRepDataConTy
  , floatRepDataConTy, doubleRepDataConTy
  , vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy
  , vec64DataConTy
  , int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy
  , int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy
  , word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy
  , doubleElemRepDataConTy
  , mkPromotedListTy, multiplicityTy )

import GHC.Types.Var    ( TyVar, mkTyVar )
import GHC.Types.Name
import {-# SOURCE #-} GHC.Types.TyThing
import GHC.Core.TyCon
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Builtin.Uniques
import GHC.Builtin.Names
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Core.TyCo.Rep -- Doesn't need special access, but this is easier to avoid
                         -- import loops which show up if you import Type instead
import {-# SOURCE #-} GHC.Core.Type ( mkTyConTy, tYPE )

import Data.Char

{-
************************************************************************
*                                                                      *
\subsection{Primitive type constructors}
*                                                                      *
************************************************************************
-}

primTyCons :: [TyCon]
primTyCons = unexposedPrimTyCons ++ exposedPrimTyCons

-- | Primitive 'TyCon's that are defined in GHC.Prim but not exposed.
-- It's important to keep these separate as we don't want users to be able to
-- write them (see #15209) or see them in GHCi's @:browse@ output
-- (see #12023).
unexposedPrimTyCons :: [TyCon]
unexposedPrimTyCons
  = [ eqPrimTyCon
    , eqReprPrimTyCon
    , eqPhantPrimTyCon
    ]

-- | Primitive 'TyCon's that are defined in, and exported from, GHC.Prim.
exposedPrimTyCons :: [TyCon]
exposedPrimTyCons
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , arrayArrayPrimTyCon
    , smallArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , int8PrimTyCon
    , int16PrimTyCon
    , int32PrimTyCon
    , int64PrimTyCon
    , bcoPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , mutableArrayArrayPrimTyCon
    , smallMutableArrayPrimTyCon
    , mVarPrimTyCon
    , ioPortPrimTyCon
    , tVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , compactPrimTyCon
    , statePrimTyCon
    , proxyPrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word8PrimTyCon
    , word16PrimTyCon
    , word32PrimTyCon
    , word64PrimTyCon

    , tYPETyCon
    , funTyCon

#include "primop-vector-tycons.hs-incl"
    ]

mkPrimTc :: FastString -> Unique -> TyCon -> Name
mkPrimTc fs unique tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS fs)
                  unique
                  (mkATyCon tycon)        -- Relevant TyCon
                  UserSyntax

mkBuiltInPrimTc :: FastString -> Unique -> TyCon -> Name
mkBuiltInPrimTc fs unique tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS fs)
                  unique
                  (mkATyCon tycon)        -- Relevant TyCon
                  BuiltInSyntax


charPrimTyConName, intPrimTyConName, int8PrimTyConName, int16PrimTyConName, int32PrimTyConName, int64PrimTyConName, wordPrimTyConName, word32PrimTyConName, word8PrimTyConName, word16PrimTyConName, word64PrimTyConName, addrPrimTyConName, floatPrimTyConName, doublePrimTyConName, statePrimTyConName, proxyPrimTyConName, realWorldTyConName, arrayPrimTyConName, arrayArrayPrimTyConName, smallArrayPrimTyConName, byteArrayPrimTyConName, mutableArrayPrimTyConName, mutableByteArrayPrimTyConName, mutableArrayArrayPrimTyConName, smallMutableArrayPrimTyConName, mutVarPrimTyConName, mVarPrimTyConName, ioPortPrimTyConName, tVarPrimTyConName, stablePtrPrimTyConName, stableNamePrimTyConName, compactPrimTyConName, bcoPrimTyConName, weakPrimTyConName, threadIdPrimTyConName, eqPrimTyConName, eqReprPrimTyConName, eqPhantPrimTyConName :: Name
charPrimTyConName             = mkPrimTc (fsLit "Char#") charPrimTyConKey charPrimTyCon
intPrimTyConName              = mkPrimTc (fsLit "Int#") intPrimTyConKey  intPrimTyCon
int8PrimTyConName             = mkPrimTc (fsLit "Int8#") int8PrimTyConKey int8PrimTyCon
int16PrimTyConName            = mkPrimTc (fsLit "Int16#") int16PrimTyConKey int16PrimTyCon
int32PrimTyConName            = mkPrimTc (fsLit "Int32#") int32PrimTyConKey int32PrimTyCon
int64PrimTyConName            = mkPrimTc (fsLit "Int64#") int64PrimTyConKey int64PrimTyCon
wordPrimTyConName             = mkPrimTc (fsLit "Word#") wordPrimTyConKey wordPrimTyCon
word8PrimTyConName            = mkPrimTc (fsLit "Word8#") word8PrimTyConKey word8PrimTyCon
word16PrimTyConName           = mkPrimTc (fsLit "Word16#") word16PrimTyConKey word16PrimTyCon
word32PrimTyConName           = mkPrimTc (fsLit "Word32#") word32PrimTyConKey word32PrimTyCon
word64PrimTyConName           = mkPrimTc (fsLit "Word64#") word64PrimTyConKey word64PrimTyCon
addrPrimTyConName             = mkPrimTc (fsLit "Addr#") addrPrimTyConKey addrPrimTyCon
floatPrimTyConName            = mkPrimTc (fsLit "Float#") floatPrimTyConKey floatPrimTyCon
doublePrimTyConName           = mkPrimTc (fsLit "Double#") doublePrimTyConKey doublePrimTyCon
statePrimTyConName            = mkPrimTc (fsLit "State#") statePrimTyConKey statePrimTyCon
proxyPrimTyConName            = mkPrimTc (fsLit "Proxy#") proxyPrimTyConKey proxyPrimTyCon
eqPrimTyConName               = mkPrimTc (fsLit "~#") eqPrimTyConKey eqPrimTyCon
eqReprPrimTyConName           = mkBuiltInPrimTc (fsLit "~R#") eqReprPrimTyConKey eqReprPrimTyCon
eqPhantPrimTyConName          = mkBuiltInPrimTc (fsLit "~P#") eqPhantPrimTyConKey eqPhantPrimTyCon
realWorldTyConName            = mkPrimTc (fsLit "RealWorld") realWorldTyConKey realWorldTyCon
arrayPrimTyConName            = mkPrimTc (fsLit "Array#") arrayPrimTyConKey arrayPrimTyCon
byteArrayPrimTyConName        = mkPrimTc (fsLit "ByteArray#") byteArrayPrimTyConKey byteArrayPrimTyCon
arrayArrayPrimTyConName       = mkPrimTc (fsLit "ArrayArray#") arrayArrayPrimTyConKey arrayArrayPrimTyCon
smallArrayPrimTyConName       = mkPrimTc (fsLit "SmallArray#") smallArrayPrimTyConKey smallArrayPrimTyCon
mutableArrayPrimTyConName     = mkPrimTc (fsLit "MutableArray#") mutableArrayPrimTyConKey mutableArrayPrimTyCon
mutableByteArrayPrimTyConName = mkPrimTc (fsLit "MutableByteArray#") mutableByteArrayPrimTyConKey mutableByteArrayPrimTyCon
mutableArrayArrayPrimTyConName= mkPrimTc (fsLit "MutableArrayArray#") mutableArrayArrayPrimTyConKey mutableArrayArrayPrimTyCon
smallMutableArrayPrimTyConName= mkPrimTc (fsLit "SmallMutableArray#") smallMutableArrayPrimTyConKey smallMutableArrayPrimTyCon
mutVarPrimTyConName           = mkPrimTc (fsLit "MutVar#") mutVarPrimTyConKey mutVarPrimTyCon
ioPortPrimTyConName           = mkPrimTc (fsLit "IOPort#") ioPortPrimTyConKey ioPortPrimTyCon
mVarPrimTyConName             = mkPrimTc (fsLit "MVar#") mVarPrimTyConKey mVarPrimTyCon
tVarPrimTyConName             = mkPrimTc (fsLit "TVar#") tVarPrimTyConKey tVarPrimTyCon
stablePtrPrimTyConName        = mkPrimTc (fsLit "StablePtr#") stablePtrPrimTyConKey stablePtrPrimTyCon
stableNamePrimTyConName       = mkPrimTc (fsLit "StableName#") stableNamePrimTyConKey stableNamePrimTyCon
compactPrimTyConName          = mkPrimTc (fsLit "Compact#") compactPrimTyConKey compactPrimTyCon

#if MIN_VERSION_ghc_prim(0, 7, 0)
bcoPrimTyConName              = mkPrimTc (fsLit "BCO") bcoPrimTyConKey bcoPrimTyCon
#else
bcoPrimTyConName              = mkPrimTc (fsLit "BCO#") bcoPrimTyConKey bcoPrimTyCon
#endif

weakPrimTyConName             = mkPrimTc (fsLit "Weak#") weakPrimTyConKey weakPrimTyCon
threadIdPrimTyConName         = mkPrimTc (fsLit "ThreadId#") threadIdPrimTyConKey threadIdPrimTyCon

{-
************************************************************************
*                                                                      *
\subsection{Support code}
*                                                                      *
************************************************************************

alphaTyVars is a list of type variables for use in templates:
        ["a", "b", ..., "z", "t1", "t2", ... ]
-}

mkTemplateKindVar :: Kind -> TyVar
mkTemplateKindVar = mkTyVar (mk_tv_name 0 "k")

mkTemplateKindVars :: [Kind] -> [TyVar]
-- k0  with unique (mkAlphaTyVarUnique 0)
-- k1  with unique (mkAlphaTyVarUnique 1)
-- ... etc
mkTemplateKindVars [kind] = [mkTemplateKindVar kind]
  -- Special case for one kind: just "k"
mkTemplateKindVars kinds
  = [ mkTyVar (mk_tv_name u ('k' : show u)) kind
    | (kind, u) <- kinds `zip` [0..] ]
mk_tv_name :: Int -> String -> Name
mk_tv_name u s = mkInternalName (mkAlphaTyVarUnique u)
                                (mkTyVarOccFS (mkFastString s))
                                noSrcSpan

mkTemplateTyVarsFrom :: Int -> [Kind] -> [TyVar]
-- a  with unique (mkAlphaTyVarUnique n)
-- b  with unique (mkAlphaTyVarUnique n+1)
-- ... etc
-- Typically called as
--   mkTemplateTyVarsFrom (length kv_bndrs) kinds
-- where kv_bndrs are the kind-level binders of a TyCon
mkTemplateTyVarsFrom n kinds
  = [ mkTyVar name kind
    | (kind, index) <- zip kinds [0..],
      let ch_ord = index + ord 'a'
          name_str | ch_ord <= ord 'z' = [chr ch_ord]
                   | otherwise         = 't':show index
          name = mk_tv_name (index + n) name_str
    ]

mkTemplateTyVars :: [Kind] -> [TyVar]
mkTemplateTyVars = mkTemplateTyVarsFrom 1

mkTemplateTyConBinders
    :: [Kind]                -- [k1, .., kn]   Kinds of kind-forall'd vars
    -> ([Kind] -> [Kind])    -- Arg is [kv1:k1, ..., kvn:kn]
                             --     same length as first arg
                             -- Result is anon arg kinds
    -> [TyConBinder]
mkTemplateTyConBinders kind_var_kinds mk_anon_arg_kinds
  = kv_bndrs ++ tv_bndrs
  where
    kv_bndrs   = mkTemplateKindTyConBinders kind_var_kinds
    anon_kinds = mk_anon_arg_kinds (mkTyVarTys (binderVars kv_bndrs))
    tv_bndrs   = mkTemplateAnonTyConBindersFrom (length kv_bndrs) anon_kinds

mkTemplateKiTyVars
    :: [Kind]                -- [k1, .., kn]   Kinds of kind-forall'd vars
    -> ([Kind] -> [Kind])    -- Arg is [kv1:k1, ..., kvn:kn]
                             --     same length as first arg
                             -- Result is anon arg kinds [ak1, .., akm]
    -> [TyVar]   -- [kv1:k1, ..., kvn:kn, av1:ak1, ..., avm:akm]
-- Example: if you want the tyvars for
--   forall (r:RuntimeRep) (a:TYPE r) (b:*). blah
-- call mkTemplateKiTyVars [RuntimeRep] (\[r] -> [TYPE r, *])
mkTemplateKiTyVars kind_var_kinds mk_arg_kinds
  = kv_bndrs ++ tv_bndrs
  where
    kv_bndrs   = mkTemplateKindVars kind_var_kinds
    anon_kinds = mk_arg_kinds (mkTyVarTys kv_bndrs)
    tv_bndrs   = mkTemplateTyVarsFrom (length kv_bndrs) anon_kinds

mkTemplateKiTyVar
    :: Kind                  -- [k1, .., kn]   Kind of kind-forall'd var
    -> (Kind -> [Kind])      -- Arg is kv1:k1
                             -- Result is anon arg kinds [ak1, .., akm]
    -> [TyVar]   -- [kv1:k1, ..., kvn:kn, av1:ak1, ..., avm:akm]
-- Example: if you want the tyvars for
--   forall (r:RuntimeRep) (a:TYPE r) (b:*). blah
-- call mkTemplateKiTyVar RuntimeRep (\r -> [TYPE r, *])
mkTemplateKiTyVar kind mk_arg_kinds
  = kv_bndr : tv_bndrs
  where
    kv_bndr    = mkTemplateKindVar kind
    anon_kinds = mk_arg_kinds (mkTyVarTy kv_bndr)
    tv_bndrs   = mkTemplateTyVarsFrom 1 anon_kinds

mkTemplateKindTyConBinders :: [Kind] -> [TyConBinder]
-- Makes named, Specified binders
mkTemplateKindTyConBinders kinds = [mkNamedTyConBinder Specified tv | tv <- mkTemplateKindVars kinds]

mkTemplateAnonTyConBinders :: [Kind] -> [TyConBinder]
mkTemplateAnonTyConBinders kinds = mkAnonTyConBinders VisArg (mkTemplateTyVars kinds)

mkTemplateAnonTyConBindersFrom :: Int -> [Kind] -> [TyConBinder]
mkTemplateAnonTyConBindersFrom n kinds = mkAnonTyConBinders VisArg (mkTemplateTyVarsFrom n kinds)

alphaTyVars :: [TyVar]
alphaTyVars = mkTemplateTyVars $ repeat liftedTypeKind

alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTys :: [Type]
alphaTys = mkTyVarTys alphaTyVars
alphaTy, betaTy, gammaTy, deltaTy :: Type
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

alphaTyVarsUnliftedRep :: [TyVar]
alphaTyVarsUnliftedRep = mkTemplateTyVars $ repeat (tYPE unliftedRepTy)

alphaTyVarUnliftedRep :: TyVar
(alphaTyVarUnliftedRep:_) = alphaTyVarsUnliftedRep

alphaTysUnliftedRep :: [Type]
alphaTysUnliftedRep = mkTyVarTys alphaTyVarsUnliftedRep
alphaTyUnliftedRep :: Type
(alphaTyUnliftedRep:_) = alphaTysUnliftedRep

runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep3TyVar :: TyVar
(runtimeRep1TyVar : runtimeRep2TyVar : runtimeRep3TyVar : _)
  = drop 16 (mkTemplateTyVars (repeat runtimeRepTy))  -- selects 'q','r'

runtimeRep1Ty, runtimeRep2Ty, runtimeRep3Ty :: Type
runtimeRep1Ty = mkTyVarTy runtimeRep1TyVar
runtimeRep2Ty = mkTyVarTy runtimeRep2TyVar
runtimeRep3Ty = mkTyVarTy runtimeRep3TyVar

openAlphaTyVar, openBetaTyVar, openGammaTyVar :: TyVar
-- alpha :: TYPE r1
-- beta  :: TYPE r2
-- gamma :: TYPE r3
[openAlphaTyVar,openBetaTyVar,openGammaTyVar]
  = mkTemplateTyVars [tYPE runtimeRep1Ty, tYPE runtimeRep2Ty, tYPE runtimeRep3Ty]

openAlphaTy, openBetaTy, openGammaTy :: Type
openAlphaTy = mkTyVarTy openAlphaTyVar
openBetaTy  = mkTyVarTy openBetaTyVar
openGammaTy = mkTyVarTy openGammaTyVar

multiplicityTyVar1, multiplicityTyVar2  :: TyVar
(multiplicityTyVar1 : multiplicityTyVar2 : _)
   = drop 13 (mkTemplateTyVars (repeat multiplicityTy))  -- selects 'n', 'm'


{-
************************************************************************
*                                                                      *
                FunTyCon
*                                                                      *
************************************************************************
-}

funTyConName :: Name
funTyConName = mkPrimTyConName (fsLit "FUN") funTyConKey funTyCon

-- | The @FUN@ type constructor.
--
-- @
-- FUN :: forall (m :: Multiplicity) ->
--        forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
--        TYPE rep1 -> TYPE rep2 -> *
-- @
--
-- The runtime representations quantification is left inferred. This
-- means they cannot be specified with @-XTypeApplications@.
--
-- This is a deliberate choice to allow future extensions to the
-- function arrow. To allow visible application a type synonym can be
-- defined:
--
-- @
-- type Arr :: forall (rep1 :: RuntimeRep) (rep2 :: RuntimeRep).
--             TYPE rep1 -> TYPE rep2 -> Type
-- type Arr = FUN 'Many
-- @
--
funTyCon :: TyCon
funTyCon = mkFunTyCon funTyConName tc_bndrs tc_rep_nm
  where
    -- See also unrestrictedFunTyCon
    tc_bndrs = [ mkNamedTyConBinder Required multiplicityTyVar1
               , mkNamedTyConBinder Inferred runtimeRep1TyVar
               , mkNamedTyConBinder Inferred runtimeRep2TyVar ]
               ++ mkTemplateAnonTyConBinders [ tYPE runtimeRep1Ty
                                             , tYPE runtimeRep2Ty
                                             ]
    tc_rep_nm = mkPrelTyConRepName funTyConName

{-
************************************************************************
*                                                                      *
                Kinds
*                                                                      *
************************************************************************

Note [TYPE and RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~
All types that classify values have a kind of the form (TYPE rr), where

    data RuntimeRep     -- Defined in ghc-prim:GHC.Types
      = BoxedRep Levity
      | IntRep
      | FloatRep
      .. etc ..

    data Levity = Lifted | Unlifted

    rr :: RuntimeRep

    TYPE :: RuntimeRep -> TYPE 'LiftedRep  -- Built in

So for example:
    Int        :: TYPE ('BoxedRep 'Lifted)
    Array# Int :: TYPE ('BoxedRep 'Unlifted)
    Int#       :: TYPE 'IntRep
    Float#     :: TYPE 'FloatRep
    Maybe      :: TYPE ('BoxedRep 'Lifted) -> TYPE ('BoxedRep 'Lifted)
    (# , #)    :: TYPE r1 -> TYPE r2 -> TYPE (TupleRep [r1, r2])

We abbreviate '*' specially:
    type LiftedRep = 'BoxedRep 'Lifted
    type * = TYPE LiftedRep

The 'rr' parameter tells us how the value is represented at runtime.

Generally speaking, you can't be polymorphic in 'rr'.  E.g
   f :: forall (rr:RuntimeRep) (a:TYPE rr). a -> [a]
   f = /\(rr:RuntimeRep) (a:rr) \(a:rr). ...
This is no good: we could not generate code for 'f', because the
calling convention for 'f' varies depending on whether the argument is
a a Int, Int#, or Float#.  (You could imagine generating specialised
code, one for each instantiation of 'rr', but we don't do that.)

Certain functions CAN be runtime-rep-polymorphic, because the code
generator never has to manipulate a value of type 'a :: TYPE rr'.

* error :: forall (rr:RuntimeRep) (a:TYPE rr). String -> a
  Code generator never has to manipulate the return value.

* unsafeCoerce#, defined in Desugar.mkUnsafeCoercePair:
  Always inlined to be a no-op
     unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                             (a :: TYPE r1) (b :: TYPE r2).
                             a -> b

* Unboxed tuples, and unboxed sums, defined in GHC.Builtin.Types
  Always inlined, and hence specialised to the call site
     (#,#) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                     (a :: TYPE r1) (b :: TYPE r2).
                     a -> b -> TYPE ('TupleRep '[r1, r2])

Note [PrimRep and kindPrimRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As part of its source code, in GHC.Core.TyCon, GHC has
  data PrimRep = BoxedRep Levity | IntRep | FloatRep | ...etc...

Notice that
 * RuntimeRep is part of the syntax tree of the program being compiled
     (defined in a library: ghc-prim:GHC.Types)
 * PrimRep is part of GHC's source code.
     (defined in GHC.Core.TyCon)

We need to get from one to the other; that is what kindPrimRep does.
Suppose we have a value
   (v :: t) where (t :: k)
Given this kind
    k = TyConApp "TYPE" [rep]
GHC needs to be able to figure out how 'v' is represented at runtime.
It expects 'rep' to be form
    TyConApp rr_dc args
where 'rr_dc' is a promoteed data constructor from RuntimeRep. So
now we need to go from 'dc' to the corresponding PrimRep.  We store this
PrimRep in the promoted data constructor itself: see TyCon.promDcRepInfo.

-}

tYPETyCon :: TyCon
tYPETyConName :: Name

tYPETyCon = mkKindTyCon tYPETyConName
                        (mkTemplateAnonTyConBinders [runtimeRepTy])
                        liftedTypeKind
                        [Nominal]
                        (mkPrelTyConRepName tYPETyConName)

--------------------------
-- ... and now their names

-- If you edit these, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
tYPETyConName             = mkPrimTyConName (fsLit "TYPE") tYPETyConKey tYPETyCon

mkPrimTyConName :: FastString -> Unique -> TyCon -> Name
mkPrimTyConName = mkPrimTcName BuiltInSyntax
  -- All of the super kinds and kinds are defined in Prim,
  -- and use BuiltInSyntax, because they are never in scope in the source

mkPrimTcName :: BuiltInSyntax -> FastString -> Unique -> TyCon -> Name
mkPrimTcName built_in_syntax occ key tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS occ) key (mkATyCon tycon) built_in_syntax

-----------------------------

-- Given a Multiplicity, applies FUN to it.
functionWithMultiplicity :: Type -> Type
functionWithMultiplicity mul = TyConApp funTyCon [mul]

{-
************************************************************************
*                                                                      *
   Basic primitive types (@Char#@, @Int#@, etc.)
*                                                                      *
************************************************************************
-}

-- only used herein
pcPrimTyCon :: Name -> [Role] -> PrimRep -> TyCon
pcPrimTyCon name roles rep
  = mkPrimTyCon name binders result_kind roles
  where
    binders     = mkTemplateAnonTyConBinders (map (const liftedTypeKind) roles)
    result_kind = tYPE (primRepToRuntimeRep rep)

-- | Convert a 'PrimRep' to a 'Type' of kind RuntimeRep
-- Defined here to avoid (more) module loops
primRepToRuntimeRep :: PrimRep -> Type
primRepToRuntimeRep rep = case rep of
  VoidRep       -> TyConApp tupleRepDataConTyCon [mkPromotedListTy runtimeRepTy []]
  LiftedRep     -> liftedRepTy
  UnliftedRep   -> unliftedRepTy
  IntRep        -> intRepDataConTy
  Int8Rep       -> int8RepDataConTy
  Int16Rep      -> int16RepDataConTy
  Int32Rep      -> int32RepDataConTy
  Int64Rep      -> int64RepDataConTy
  WordRep       -> wordRepDataConTy
  Word8Rep      -> word8RepDataConTy
  Word16Rep     -> word16RepDataConTy
  Word32Rep     -> word32RepDataConTy
  Word64Rep     -> word64RepDataConTy
  AddrRep       -> addrRepDataConTy
  FloatRep      -> floatRepDataConTy
  DoubleRep     -> doubleRepDataConTy
  VecRep n elem -> TyConApp vecRepDataConTyCon [n', elem']
    where
      n' = case n of
        2  -> vec2DataConTy
        4  -> vec4DataConTy
        8  -> vec8DataConTy
        16 -> vec16DataConTy
        32 -> vec32DataConTy
        64 -> vec64DataConTy
        _  -> pprPanic "Disallowed VecCount" (ppr n)

      elem' = case elem of
        Int8ElemRep   -> int8ElemRepDataConTy
        Int16ElemRep  -> int16ElemRepDataConTy
        Int32ElemRep  -> int32ElemRepDataConTy
        Int64ElemRep  -> int64ElemRepDataConTy
        Word8ElemRep  -> word8ElemRepDataConTy
        Word16ElemRep -> word16ElemRepDataConTy
        Word32ElemRep -> word32ElemRepDataConTy
        Word64ElemRep -> word64ElemRepDataConTy
        FloatElemRep  -> floatElemRepDataConTy
        DoubleElemRep -> doubleElemRepDataConTy

pcPrimTyCon0 :: Name -> PrimRep -> TyCon
pcPrimTyCon0 name rep
  = pcPrimTyCon name [] rep

charPrimTy :: Type
charPrimTy      = mkTyConTy charPrimTyCon
charPrimTyCon :: TyCon
charPrimTyCon   = pcPrimTyCon0 charPrimTyConName WordRep

intPrimTy :: Type
intPrimTy       = mkTyConTy intPrimTyCon
intPrimTyCon :: TyCon
intPrimTyCon    = pcPrimTyCon0 intPrimTyConName IntRep

int8PrimTy :: Type
int8PrimTy     = mkTyConTy int8PrimTyCon
int8PrimTyCon :: TyCon
int8PrimTyCon  = pcPrimTyCon0 int8PrimTyConName Int8Rep

int16PrimTy :: Type
int16PrimTy    = mkTyConTy int16PrimTyCon
int16PrimTyCon :: TyCon
int16PrimTyCon = pcPrimTyCon0 int16PrimTyConName Int16Rep

int32PrimTy :: Type
int32PrimTy     = mkTyConTy int32PrimTyCon
int32PrimTyCon :: TyCon
int32PrimTyCon  = pcPrimTyCon0 int32PrimTyConName Int32Rep

int64PrimTy :: Type
int64PrimTy     = mkTyConTy int64PrimTyCon
int64PrimTyCon :: TyCon
int64PrimTyCon  = pcPrimTyCon0 int64PrimTyConName Int64Rep

wordPrimTy :: Type
wordPrimTy      = mkTyConTy wordPrimTyCon
wordPrimTyCon :: TyCon
wordPrimTyCon   = pcPrimTyCon0 wordPrimTyConName WordRep

word8PrimTy :: Type
word8PrimTy     = mkTyConTy word8PrimTyCon
word8PrimTyCon :: TyCon
word8PrimTyCon  = pcPrimTyCon0 word8PrimTyConName Word8Rep

word16PrimTy :: Type
word16PrimTy    = mkTyConTy word16PrimTyCon
word16PrimTyCon :: TyCon
word16PrimTyCon = pcPrimTyCon0 word16PrimTyConName Word16Rep

word32PrimTy :: Type
word32PrimTy    = mkTyConTy word32PrimTyCon
word32PrimTyCon :: TyCon
word32PrimTyCon = pcPrimTyCon0 word32PrimTyConName Word32Rep

word64PrimTy :: Type
word64PrimTy    = mkTyConTy word64PrimTyCon
word64PrimTyCon :: TyCon
word64PrimTyCon = pcPrimTyCon0 word64PrimTyConName Word64Rep

addrPrimTy :: Type
addrPrimTy      = mkTyConTy addrPrimTyCon
addrPrimTyCon :: TyCon
addrPrimTyCon   = pcPrimTyCon0 addrPrimTyConName AddrRep

floatPrimTy     :: Type
floatPrimTy     = mkTyConTy floatPrimTyCon
floatPrimTyCon :: TyCon
floatPrimTyCon  = pcPrimTyCon0 floatPrimTyConName FloatRep

doublePrimTy :: Type
doublePrimTy    = mkTyConTy doublePrimTyCon
doublePrimTyCon :: TyCon
doublePrimTyCon = pcPrimTyCon0 doublePrimTyConName DoubleRep

{-
************************************************************************
*                                                                      *
   The @State#@ type (and @_RealWorld@ types)
*                                                                      *
************************************************************************

Note [The equality types story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC sports a veritable menagerie of equality types:

         Type or  Lifted?  Hetero?  Role      Built in         Defining module
         class?    L/U                        TyCon
-----------------------------------------------------------------------------------------
~#         T        U      hetero   nominal   eqPrimTyCon      GHC.Prim
~~         C        L      hetero   nominal   heqTyCon         GHC.Types
~          C        L      homo     nominal   eqTyCon          GHC.Types
:~:        T        L      homo     nominal   (not built-in)   Data.Type.Equality
:~~:       T        L      hetero   nominal   (not built-in)   Data.Type.Equality

~R#        T        U      hetero   repr      eqReprPrimTy     GHC.Prim
Coercible  C        L      homo     repr      coercibleTyCon   GHC.Types
Coercion   T        L      homo     repr      (not built-in)   Data.Type.Coercion
~P#        T        U      hetero   phantom   eqPhantPrimTyCon GHC.Prim

Recall that "hetero" means the equality can related types of different
kinds. Knowing that (t1 ~# t2) or (t1 ~R# t2) or even that (t1 ~P# t2)
also means that (k1 ~# k2), where (t1 :: k1) and (t2 :: k2).

To produce less confusion for end users, when not dumping and without
-fprint-equality-relations, each of these groups is printed as the bottommost
listed equality. That is, (~#) and (~~) are both rendered as (~) in
error messages, and (~R#) is rendered as Coercible.

Let's take these one at a time:

    --------------------------
    (~#) :: forall k1 k2. k1 -> k2 -> #
    --------------------------
This is The Type Of Equality in GHC. It classifies nominal coercions.
This type is used in the solver for recording equality constraints.
It responds "yes" to Type.isEqPrimPred and classifies as an EqPred in
Type.classifyPredType.

All wanted constraints of this type are built with coercion holes.
(See Note [Coercion holes] in GHC.Core.TyCo.Rep.) But see also
Note [Deferred errors for coercion holes] in GHC.Tc.Errors to see how
equality constraints are deferred.

Within GHC, ~# is called eqPrimTyCon, and it is defined in GHC.Builtin.Types.Prim.


    --------------------------
    (~~) :: forall k1 k2. k1 -> k2 -> Constraint
    --------------------------
This is (almost) an ordinary class, defined as if by
  class a ~# b => a ~~ b
  instance a ~# b => a ~~ b
Here's what's unusual about it:

 * We can't actually declare it that way because we don't have syntax for ~#.
   And ~# isn't a constraint, so even if we could write it, it wouldn't kind
   check.

 * Users cannot write instances of it.

 * It is "naturally coherent". This means that the solver won't hesitate to
   solve a goal of type (a ~~ b) even if there is, say (Int ~~ c) in the
   context. (Normally, it waits to learn more, just in case the given
   influences what happens next.) See Note [Naturally coherent classes]
   in GHC.Tc.Solver.Interact.

 * It always terminates. That is, in the UndecidableInstances checks, we
   don't worry if a (~~) constraint is too big, as we know that solving
   equality terminates.

On the other hand, this behaves just like any class w.r.t. eager superclass
unpacking in the solver. So a lifted equality given quickly becomes an unlifted
equality given. This is good, because the solver knows all about unlifted
equalities. There is some special-casing in GHC.Tc.Solver.Interact.matchClassInst to
pretend that there is an instance of this class, as we can't write the instance
in Haskell.

Within GHC, ~~ is called heqTyCon, and it is defined in GHC.Builtin.Types.


    --------------------------
    (~) :: forall k. k -> k -> Constraint
    --------------------------
This is /exactly/ like (~~), except with a homogeneous kind.
It is an almost-ordinary class defined as if by
  class a ~# b => (a :: k) ~ (b :: k)
  instance a ~# b => a ~ b

 * All the bullets for (~~) apply

 * In addition (~) is magical syntax, as ~ is a reserved symbol.
   It cannot be exported or imported.

Within GHC, ~ is called eqTyCon, and it is defined in GHC.Builtin.Types.

Historical note: prior to July 18 (~) was defined as a
  more-ordinary class with (~~) as a superclass.  But that made it
  special in different ways; and the extra superclass selections to
  get from (~) to (~#) via (~~) were tiresome.  Now it's defined
  uniformly with (~~) and Coercible; much nicer.)


    --------------------------
    (:~:) :: forall k. k -> k -> *
    (:~~:) :: forall k1 k2. k1 -> k2 -> *
    --------------------------
These are perfectly ordinary GADTs, wrapping (~) and (~~) resp.
They are not defined within GHC at all.


    --------------------------
    (~R#) :: forall k1 k2. k1 -> k2 -> #
    --------------------------
The is the representational analogue of ~#. This is the type of representational
equalities that the solver works on. All wanted constraints of this type are
built with coercion holes.

Within GHC, ~R# is called eqReprPrimTyCon, and it is defined in GHC.Builtin.Types.Prim.


    --------------------------
    Coercible :: forall k. k -> k -> Constraint
    --------------------------
This is quite like (~~) in the way it's defined and treated within GHC, but
it's homogeneous. Homogeneity helps with type inference (as GHC can solve one
kind from the other) and, in my (Richard's) estimation, will be more intuitive
for users.

An alternative design included HCoercible (like (~~)) and Coercible (like (~)).
One annoyance was that we want `coerce :: Coercible a b => a -> b`, and
we need the type of coerce to be fully wired-in. So the HCoercible/Coercible
split required that both types be fully wired-in. Instead of doing this,
I just got rid of HCoercible, as I'm not sure who would use it, anyway.

Within GHC, Coercible is called coercibleTyCon, and it is defined in
GHC.Builtin.Types.


    --------------------------
    Coercion :: forall k. k -> k -> *
    --------------------------
This is a perfectly ordinary GADT, wrapping Coercible. It is not defined
within GHC at all.


    --------------------------
    (~P#) :: forall k1 k2. k1 -> k2 -> #
    --------------------------
This is the phantom analogue of ~# and it is barely used at all.
(The solver has no idea about this one.) Here is the motivation:

    data Phant a = MkPhant
    type role Phant phantom

    Phant <Int, Bool>_P :: Phant Int ~P# Phant Bool

We just need to have something to put on that last line. You probably
don't need to worry about it.



Note [The State# TyCon]
~~~~~~~~~~~~~~~~~~~~~~~
State# is the primitive, unlifted type of states.  It has one type parameter,
thus
        State# RealWorld
or
        State# s

where s is a type variable. The only purpose of the type parameter is to
keep different state threads separate.  It is represented by nothing at all.

The type parameter to State# is intended to keep separate threads separate.
Even though this parameter is not used in the definition of State#, it is
given role Nominal to enforce its intended use.
-}

mkStatePrimTy :: Type -> Type
mkStatePrimTy ty = TyConApp statePrimTyCon [ty]

statePrimTyCon :: TyCon   -- See Note [The State# TyCon]
statePrimTyCon   = pcPrimTyCon statePrimTyConName [Nominal] VoidRep

{-
RealWorld is deeply magical.  It is *primitive*, but it is not
*unlifted* (hence ptrArg).  We never manipulate values of type
RealWorld; it's only used in the type system, to parameterise State#.
-}

realWorldTyCon :: TyCon
realWorldTyCon = mkLiftedPrimTyCon realWorldTyConName [] liftedTypeKind []
realWorldTy :: Type
realWorldTy          = mkTyConTy realWorldTyCon
realWorldStatePrimTy :: Type
realWorldStatePrimTy = mkStatePrimTy realWorldTy        -- State# RealWorld

-- Note: the ``state-pairing'' types are not truly primitive,
-- so they are defined in \tr{GHC.Builtin.Types}, not here.


mkProxyPrimTy :: Type -> Type -> Type
mkProxyPrimTy k ty = TyConApp proxyPrimTyCon [k, ty]

proxyPrimTyCon :: TyCon
proxyPrimTyCon = mkPrimTyCon proxyPrimTyConName binders res_kind [Nominal,Phantom]
  where
     -- Kind: forall k. k -> TYPE (Tuple '[])
     binders = mkTemplateTyConBinders [liftedTypeKind] id
     res_kind = unboxedTupleKind []


{- *********************************************************************
*                                                                      *
                Primitive equality constraints
    See Note [The equality types story]
*                                                                      *
********************************************************************* -}

eqPrimTyCon :: TyCon  -- The representation type for equality predicates
                      -- See Note [The equality types story]
eqPrimTyCon  = mkPrimTyCon eqPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> TYPE (Tuple '[])
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = unboxedTupleKind []
    roles    = [Nominal, Nominal, Nominal, Nominal]

-- like eqPrimTyCon, but the type for *Representational* coercions
-- this should only ever appear as the type of a covar. Its role is
-- interpreted in coercionRole
eqReprPrimTyCon :: TyCon   -- See Note [The equality types story]
eqReprPrimTyCon = mkPrimTyCon eqReprPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> TYPE (Tuple '[])
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = unboxedTupleKind []
    roles    = [Nominal, Nominal, Representational, Representational]

-- like eqPrimTyCon, but the type for *Phantom* coercions.
-- This is only used to make higher-order equalities. Nothing
-- should ever actually have this type!
eqPhantPrimTyCon :: TyCon
eqPhantPrimTyCon = mkPrimTyCon eqPhantPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> TYPE (Tuple '[])
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = unboxedTupleKind []
    roles    = [Nominal, Nominal, Phantom, Phantom]

-- | Given a Role, what TyCon is the type of equality predicates at that role?
equalityTyCon :: Role -> TyCon
equalityTyCon Nominal          = eqPrimTyCon
equalityTyCon Representational = eqReprPrimTyCon
equalityTyCon Phantom          = eqPhantPrimTyCon

{- *********************************************************************
*                                                                      *
             The primitive array types
*                                                                      *
********************************************************************* -}

arrayPrimTyCon, mutableArrayPrimTyCon, mutableByteArrayPrimTyCon,
    byteArrayPrimTyCon, arrayArrayPrimTyCon, mutableArrayArrayPrimTyCon,
    smallArrayPrimTyCon, smallMutableArrayPrimTyCon :: TyCon
arrayPrimTyCon             = pcPrimTyCon arrayPrimTyConName             [Representational] UnliftedRep
mutableArrayPrimTyCon      = pcPrimTyCon  mutableArrayPrimTyConName     [Nominal, Representational] UnliftedRep
mutableByteArrayPrimTyCon  = pcPrimTyCon mutableByteArrayPrimTyConName  [Nominal] UnliftedRep
byteArrayPrimTyCon         = pcPrimTyCon0 byteArrayPrimTyConName        UnliftedRep
arrayArrayPrimTyCon        = pcPrimTyCon0 arrayArrayPrimTyConName       UnliftedRep
mutableArrayArrayPrimTyCon = pcPrimTyCon mutableArrayArrayPrimTyConName [Nominal] UnliftedRep
smallArrayPrimTyCon        = pcPrimTyCon smallArrayPrimTyConName        [Representational] UnliftedRep
smallMutableArrayPrimTyCon = pcPrimTyCon smallMutableArrayPrimTyConName [Nominal, Representational] UnliftedRep

mkArrayPrimTy :: Type -> Type
mkArrayPrimTy elt           = TyConApp arrayPrimTyCon [elt]
byteArrayPrimTy :: Type
byteArrayPrimTy             = mkTyConTy byteArrayPrimTyCon
mkArrayArrayPrimTy :: Type
mkArrayArrayPrimTy = mkTyConTy arrayArrayPrimTyCon
mkSmallArrayPrimTy :: Type -> Type
mkSmallArrayPrimTy elt = TyConApp smallArrayPrimTyCon [elt]
mkMutableArrayPrimTy :: Type -> Type -> Type
mkMutableArrayPrimTy s elt  = TyConApp mutableArrayPrimTyCon [s, elt]
mkMutableByteArrayPrimTy :: Type -> Type
mkMutableByteArrayPrimTy s  = TyConApp mutableByteArrayPrimTyCon [s]
mkMutableArrayArrayPrimTy :: Type -> Type
mkMutableArrayArrayPrimTy s = TyConApp mutableArrayArrayPrimTyCon [s]
mkSmallMutableArrayPrimTy :: Type -> Type -> Type
mkSmallMutableArrayPrimTy s elt = TyConApp smallMutableArrayPrimTyCon [s, elt]


{- *********************************************************************
*                                                                      *
                The mutable variable type
*                                                                      *
********************************************************************* -}

mutVarPrimTyCon :: TyCon
mutVarPrimTyCon = pcPrimTyCon mutVarPrimTyConName [Nominal, Representational] UnliftedRep

mkMutVarPrimTy :: Type -> Type -> Type
mkMutVarPrimTy s elt        = TyConApp mutVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-io-port-var]{The synchronizing I/O Port type}
*                                                                      *
************************************************************************
-}

ioPortPrimTyCon :: TyCon
ioPortPrimTyCon = pcPrimTyCon ioPortPrimTyConName [Nominal, Representational] UnliftedRep

mkIOPortPrimTy :: Type -> Type -> Type
mkIOPortPrimTy s elt          = TyConApp ioPortPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
   The synchronizing variable type
\subsection[TysPrim-synch-var]{The synchronizing variable type}
*                                                                      *
************************************************************************
-}

mVarPrimTyCon :: TyCon
mVarPrimTyCon = pcPrimTyCon mVarPrimTyConName [Nominal, Representational] UnliftedRep

mkMVarPrimTy :: Type -> Type -> Type
mkMVarPrimTy s elt          = TyConApp mVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
   The transactional variable type
*                                                                      *
************************************************************************
-}

tVarPrimTyCon :: TyCon
tVarPrimTyCon = pcPrimTyCon tVarPrimTyConName [Nominal, Representational] UnliftedRep

mkTVarPrimTy :: Type -> Type -> Type
mkTVarPrimTy s elt = TyConApp tVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
   The stable-pointer type
*                                                                      *
************************************************************************
-}

stablePtrPrimTyCon :: TyCon
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConName [Representational] AddrRep

mkStablePtrPrimTy :: Type -> Type
mkStablePtrPrimTy ty = TyConApp stablePtrPrimTyCon [ty]

{-
************************************************************************
*                                                                      *
   The stable-name type
*                                                                      *
************************************************************************
-}

stableNamePrimTyCon :: TyCon
stableNamePrimTyCon = pcPrimTyCon stableNamePrimTyConName [Phantom] UnliftedRep

mkStableNamePrimTy :: Type -> Type
mkStableNamePrimTy ty = TyConApp stableNamePrimTyCon [ty]

{-
************************************************************************
*                                                                      *
   The Compact NFData (CNF) type
*                                                                      *
************************************************************************
-}

compactPrimTyCon :: TyCon
compactPrimTyCon = pcPrimTyCon0 compactPrimTyConName UnliftedRep

compactPrimTy :: Type
compactPrimTy = mkTyConTy compactPrimTyCon

{-
************************************************************************
*                                                                      *
   The ``bytecode object'' type
*                                                                      *
************************************************************************
-}

-- Unlike most other primitive types, BCO is lifted. This is because in
-- general a BCO may be a thunk for the reasons given in Note [Updatable CAF
-- BCOs] in GHCi.CreateBCO.
bcoPrimTy    :: Type
bcoPrimTy    = mkTyConTy bcoPrimTyCon
bcoPrimTyCon :: TyCon

#if MIN_VERSION_ghc_prim(0, 7, 0)
bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName LiftedRep
#else
bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName UnliftedRep
#endif


{-
************************************************************************
*                                                                      *
   The ``weak pointer'' type
*                                                                      *
************************************************************************
-}

weakPrimTyCon :: TyCon
weakPrimTyCon = pcPrimTyCon weakPrimTyConName [Representational] UnliftedRep

mkWeakPrimTy :: Type -> Type
mkWeakPrimTy v = TyConApp weakPrimTyCon [v]

{-
************************************************************************
*                                                                      *
   The ``thread id'' type
*                                                                      *
************************************************************************

A thread id is represented by a pointer to the TSO itself, to ensure
that they are always unique and we can always find the TSO for a given
thread id.  However, this has the unfortunate consequence that a
ThreadId# for a given thread is treated as a root by the garbage
collector and can keep TSOs around for too long.

Hence the programmer API for thread manipulation uses a weak pointer
to the thread id internally.
-}

threadIdPrimTy :: Type
threadIdPrimTy    = mkTyConTy threadIdPrimTyCon
threadIdPrimTyCon :: TyCon
threadIdPrimTyCon = pcPrimTyCon0 threadIdPrimTyConName UnliftedRep

{-
************************************************************************
*                                                                      *
\subsection{SIMD vector types}
*                                                                      *
************************************************************************
-}

#include "primop-vector-tys.hs-incl"
