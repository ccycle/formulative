{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs where

import Control.Applicative (liftA2)
import Control.Exception.Safe (Typeable)
import Data.Constraint
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import Data.Proxy
import Data.Singletons
import Data.Singletons.Prelude.List (SList)
import Data.Singletons.TH hiding (type (<=))
import Data.Type.Equality
import qualified Data.Vector.Storable as VST
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.Internal.TypeLevelList
import Formulative.Calculation.Internal.TypeLevelNatural
import Formulative.Calculation.Matrix.Class
import Formulative.Calculation.VectorSpace.Class
import GHC.Natural
import GHC.TypeLits (Symbol)
import GHC.TypeLits.KnownNat
import GHC.TypeNats
import Unsafe.Coerce

sameCellType :: forall (c :: CellType) (c' :: CellType). (SingI c, SingI c') => Sing c -> Sing c' -> Maybe (c :~: c')
sameCellType singc singc'
    | fromSing singc == fromSing singc' = Just (unsafeCoerce Refl)
    | otherwise = Nothing

type family DualMap (c :: CellType) = r | r -> c where
    DualMap 'Primal = 'Dual
    DualMap 'Dual = 'Primal
    DualMap 'CEmpty = 'CEmpty

dualMap :: CellType -> CellType
dualMap Primal = Dual
dualMap Dual = Primal
dualMap CEmpty = CEmpty

singletons
    [d|
        data OperatorType
            = DifferentialFormType
            | ExteriorDerivativeType
            | HodgeStarType
            | InteriorProductType
            | LieDerivativeType
            | CodifferentialType
            | LaplacianType
            | InclusionMapType
            deriving (Show)
        |]

-- size : p2*p1 のうちp2の部分を取得
type family ToMatSizeFromType n l c k t where
    ToMatSizeFromType n l c k DifferentialFormType = (ToMatSize n l c k)
    ToMatSizeFromType n l c k ExteriorDerivativeType = (ToMatSize n l c (SuccDeg n k))
    ToMatSizeFromType n l c k InteriorProductType = (ToMatSize n l c (PredDeg n n))
    ToMatSizeFromType n l c k LieDerivativeType = (ToMatSize n l c k)
    ToMatSizeFromType n l c _ InclusionMapType = (ToMatSize n l c (PredDeg n n))
    ToMatSizeFromType n l c k HodgeStarType = (ToMatSize n l (DualMap c) (DualDeg n k))
    ToMatSizeFromType n l c k CodifferentialType = (ToMatSize n l c (PredDeg n k))
    ToMatSizeFromType n l c k LaplacianType = (ToMatSize n l c k)

-- H^k ~ H_(n-k)
poincareDuality :: forall n l c k. (ToMatSize n l (DualMap c) (DualDeg n k) :~: ToMatSize n l c k)
poincareDuality = unsafeCoerce Refl

-- H^k ~ H_((n-(k+1))+1)
poincareDualitySucc :: forall n l c k. (ToMatSize n l (DualMap c) (SuccDeg n (DualDeg n (SuccDeg n k))) :~: ToMatSize n l c k)
poincareDualitySucc = unsafeCoerce Refl

-- nonzeroLEQProof :: Proxy n -> Proxy k -> (k :~: DualDeg n (DualDeg n k))
-- nonzeroLEQProof = unsafeCoerce Refl
dualityLaw :: forall c. (c :~: DualMap (DualMap c))
dualityLaw = unsafeCoerce Refl

dualityLawNat :: forall n k. Proxy n -> Proxy k -> (k :~: DualDeg n (DualDeg n k))
dualityLawNat _ _ = unsafeCoerce Refl

-- idempotentMod :: Proxy n -> Proxy k -> (Mod k n :~: Mod (Mod k n) n)
-- idempotentMod _ _ = unsafeCoerce Refl

axiom :: Dict c
axiom = unsafeCoerce (Dict :: Dict ())

-- leqLaw :: KnownNat k => (k ~ 0) :- (k <= 0)
-- leqLaw = Sub axiom

-- leqLaw2 :: KnownNat k => (k <= 0) :- (k ~ 0)
-- leqLaw2 = Sub axiom

-- https://hackage.haskell.org/package/constraints-0.13.3/docs/src/Data.Constraint.Nat.html#Magic
-- newtype Magic n = Magic (KnownNat n => Dict (KnownNat n))

-- magic :: forall n m o. (Natural -> Natural -> Natural) -> (KnownNat n, KnownNat m) :- KnownNat o
-- magic f = Sub $ unsafeCoerce (Magic Dict) (natVal (Proxy :: Proxy n) `f` natVal (Proxy :: Proxy m))

newtype MagicList (l :: [Nat]) = MagicList (SingI l => Dict (SingI l))

-----------
newtype MagicCell (n :: CellType) = MagicCell (SingI n => Dict (SingI n))

magicCellType :: forall (n :: CellType) (o :: CellType). (CellType -> CellType) -> SingI n :- SingI o
magicCellType f = Sub $ unsafeCoerce (MagicCell Dict) (f (fromSing (Sing :: SCellType n)))

-- `case dualMapDict @c of Sub Dict -> ...`
dualMapDict :: forall (c :: CellType). SingI c :- SingI (DualMap c)
dualMapDict = magicCellType dualMap

toMatSize :: Natural -> [Natural] -> CellType -> Natural -> Natural
toMatSize n l Primal k = l !!! k
toMatSize n l Dual k = l !!! dualDeg n k
toMatSize n l CEmpty k = k

dualDeg :: Natural -> Natural -> Natural
dualDeg n k = mod (n + 2 + n - k) (n + 2)
dualDegDict :: forall n k. (KnownNat n, KnownNat k) => Proxy n -> Proxy k -> Dict (KnownNat (DualDeg n k))
dualDegDict proxyn proxyk = let n = natVal proxyn; k = natVal proxyk; val = dualDeg n k in case someNatVal val of SomeNat p -> unsafeCoerce (natDict p)

-- TODO: modを使わない形にする
--  素直にk+1などを使ったほうがコンパイラが式変形しやすい
succDeg :: Natural -> Natural -> Natural
succDeg n k = mod (k + 1) (n + 2)
succDegDict :: forall n k. (KnownNat n, KnownNat k) => Dict (KnownNat (SuccDeg n k))
succDegDict = let proxyn = Proxy :: Proxy n; proxyk = Proxy :: Proxy k; n = natVal proxyn; k = natVal proxyk; val = succDeg n k in case someNatVal val of SomeNat p -> unsafeCoerce (natDict p)

predDeg :: Natural -> Natural -> Natural
predDeg n k = mod (n + k + 1) (n + 2)
predDegDict :: forall n k. (KnownNat n, KnownNat k) => Dict (KnownNat (PredDeg n k))
predDegDict = let proxyn = Proxy :: Proxy n; proxyk = Proxy :: Proxy k; n = natVal proxyn; k = natVal proxyk; val = predDeg n k in case someNatVal val of SomeNat p -> unsafeCoerce (natDict p)

dualPredDeg n k = dualDeg n (predDeg n k)
dualPredDegDict :: forall n k. (KnownNat n, KnownNat k) => Dict (KnownNat (DualPredDeg n k))
dualPredDegDict = let n = natVal (Proxy :: Proxy n); k = natVal (Proxy :: Proxy k); val = dualPredDeg n k in case someNatVal val of SomeNat p -> unsafeCoerce (natDict p)
dualSuccDeg n k = dualDeg n (succDeg n k)
dualSuccDegDict :: forall n k. (KnownNat n, KnownNat k) => Dict (KnownNat (DualSuccDeg n k))
dualSuccDegDict = let n = natVal (Proxy :: Proxy n); k = natVal (Proxy :: Proxy k); val = dualSuccDeg n k in case someNatVal val of SomeNat p -> unsafeCoerce (natDict p)

-- term-level function
toMatSizeFromType :: Natural -> [Natural] -> CellType -> Natural -> OperatorType -> Natural
toMatSizeFromType n l c k DifferentialFormType = toMatSize n l c k
toMatSizeFromType n l c k ExteriorDerivativeType = toMatSize n l c (succDeg n k)
toMatSizeFromType n l c k LieDerivativeType = toMatSize n l c k
toMatSizeFromType n l c k InteriorProductType = toMatSize n l c (predDeg n n)
toMatSizeFromType n l c _ InclusionMapType = toMatSize n l c (predDeg n n)
toMatSizeFromType n l c k CodifferentialType = toMatSize n l c (predDeg n k)
toMatSizeFromType n l c k HodgeStarType = toMatSize n l (dualMap c) (dualDeg n k)
toMatSizeFromType n l c k LaplacianType = toMatSize n l c k

-- KnownNat (ToMatSizeFromType n l c k t) の導出
toMatSizeFromTypeDict :: forall n l c k t. (KnownNat n, SingI l, SingI c, KnownNat k, SingI t) => Dict (KnownNat (ToMatSizeFromType n l c k t))
toMatSizeFromTypeDict =
    let proxyn = Proxy :: Proxy n
        proxyl = Sing :: SList l
        proxyc = Sing :: SCellType c
        proxyk = Proxy :: Proxy k
        proxyt = Sing :: Sing t
        l = fromSing proxyl
        n = natVal proxyn
        c = fromSing proxyc
        k = natVal proxyk
        t = fromSing proxyt
        val = toMatSizeFromType n l c k t
     in case someNatVal val of
            (SomeNat p3) -> unsafeCoerce (natDict p3)

-- KnownNat (ToMatSize n l c k) の導出
toMatSizeDict :: forall n l c k. (KnownNat n, SingI l, SingI c, KnownNat k) => Dict (KnownNat (ToMatSize n l c k))
toMatSizeDict =
    let proxyn = Proxy :: Proxy n
        proxyl = Sing :: SList l
        proxyc = Sing :: SCellType c
        proxyk = Proxy :: Proxy k
        l = fromSing proxyl
        n = natVal proxyn
        c = fromSing proxyc
        k = natVal proxyk
        val = toMatSize n l c k
     in case someNatVal val of
            (SomeNat p) -> unsafeCoerce (natDict p)

-- KnownMatSize n l c k t の導出
knownMatSizeDict :: forall n l c k t. (KnownNat n, SingI l, SingI c, KnownNat k, SingI t) => (Dict (KnownNat (ToMatSizeFromType n l c k t)), Dict (KnownNat (ToMatSize n l c k)))
knownMatSizeDict = (toMatSizeFromTypeDict @n @l @c @k @t, toMatSizeDict @n @l @c @k)

knownPointDataSizePrimal0Dict :: forall l. (SingI l) => SList l -> Dict (KnownNat (l !! 0))
knownPointDataSizePrimal0Dict proxyl =
    let l = fromSing proxyl; val = l !!! 0
     in case someNatVal val of
            (SomeNat p) -> unsafeCoerce (natDict p)

-- 自動でKnownNatを導出
-- https://hackage.haskell.org/package/ghc-typelits-knownnat-0.7.6/docs/GHC-TypeLits-KnownNat.html
instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''DualDeg) a b where
    natSing2 =
        let x = natVal (Proxy :: Proxy a)
            y = natVal (Proxy :: Proxy b)
            z = dualDeg x y
         in SNatKn z
    {-# INLINE natSing2 #-}

instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''SuccDeg) a b where
    natSing2 =
        let x = natVal (Proxy :: Proxy a)
            y = natVal (Proxy :: Proxy b)
            z = succDeg x y
         in SNatKn z
    {-# INLINE natSing2 #-}
instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''PredDeg) a b where
    natSing2 =
        let x = natVal (Proxy :: Proxy a)
            y = natVal (Proxy :: Proxy b)
            z = predDeg x y
         in SNatKn z
    {-# INLINE natSing2 #-}

instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''DualPredDeg) a b where
    natSing2 =
        let x = natVal (Proxy :: Proxy a)
            y = natVal (Proxy :: Proxy b)
            z = dualPredDeg x y
         in SNatKn z
    {-# INLINE natSing2 #-}
instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''DualSuccDeg) a b where
    natSing2 =
        let x = natVal (Proxy :: Proxy a)
            y = natVal (Proxy :: Proxy b)
            z = dualSuccDeg x y
         in SNatKn z
    {-# INLINE natSing2 #-}

knownMatSizeDoublet ::
    forall n l c1 c2 k1 k2.
    ( KnownNat n
    , SingI l
    , SingI c1
    , SingI c2
    , KnownNat k1
    , KnownNat k2
    ) =>
    (Dict (KnownNat (ToMatSize n l c1 k1)), Dict (KnownNat (ToMatSize n l c2 k2)))
knownMatSizeDoublet =
    ( toMatSizeDict @n @l @c1 @k1
    , toMatSizeDict @n @l @c2 @k2
    )

knownMatSizeTriplet ::
    forall n l c1 c2 c3 k1 k2 k3.
    ( KnownNat n
    , SingI l
    , SingI c1
    , SingI c2
    , SingI c3
    , KnownNat k1
    , KnownNat k2
    , KnownNat k3
    ) =>
    ( Dict (KnownNat (ToMatSize n l c1 k1))
    , Dict (KnownNat (ToMatSize n l c2 k2))
    , Dict (KnownNat (ToMatSize n l c3 k3))
    )
knownMatSizeTriplet =
    ( toMatSizeDict @n @l @c1 @k1
    , toMatSizeDict @n @l @c2 @k2
    , toMatSizeDict @n @l @c3 @k3
    )
