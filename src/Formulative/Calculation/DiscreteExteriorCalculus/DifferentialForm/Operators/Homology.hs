{-# OPTIONS_GHC-fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC-fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators.Homology where

-- exterior derivative
-- wedge product

import Control.Algebra
import qualified Control.Lens as MSS
import Data.Coerce
import Data.Constraint
import Data.Foldable (Foldable (foldl'))
import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import qualified Data.Matrix.Static.Generic as MSG
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Singletons
import Data.Singletons.Prelude (SList)
import Data.Type.Equality
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Storable as VST
import qualified Data.Vector.Unboxed as VU
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Effect
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Operators
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import Formulative.Calculation.Internal.TypeLevelNatural
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.Natural
import GHC.TypeNats

-- import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
-- import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types

exteriorDerivativePrimalInternal ::
    forall n l k a.
    ( VU.Unbox a
    , MSL.Numeric a
    , KnownNat k
    , KnownNat n
    , KnownHDims l
    ) =>
    Simplices (k + 1) l ->
    Simplices k l ->
    ExteriorDerivative n l Primal k a
exteriorDerivativePrimalInternal sc sc' = case knownMatSizeDict @n @l @Primal @k @ExteriorDerivativeType of
    (Dict, Dict) -> DECrepresentationMatrix . MSS.fromTriplet $ boundaryOperatorMatrixCOOList sc sc'

type HasExteriorDerivative n l c k a sig m =
    ( KnownNat n
    , SingI l
    , SingI c
    , KnownNat k
    , Has (Connectivity n l) sig m
    , AdditiveGroup a
    , VU.Unbox a
    , MSL.Numeric a
    )

-- KnownNat k, KnownNat n, Algebra sig m, SingI l, SingI c, AdditiveGroup a, VU.Unbox a, MSL.Numeric a, Has (Connectivity n l) sig m

exteriorDerivativePrimalMat ::
    forall n l k a sig m.
    (SingI l, KnownNat n, KnownNat k, Algebra sig m, Has (Connectivity n l) sig m, VU.Unbox a, MSL.Numeric a) =>
    m (ExteriorDerivative n l Primal k a)
exteriorDerivativePrimalMat = case knownMatSizeDict @n @l @Primal @k @ExteriorDerivativeType of
    (Dict, Dict) ->
        let sck = getKSimplicialComplex @n @l @k
            sckPlus1 = getKSimplicialComplex @n @l @(k + 1)
         in exteriorDerivativePrimalInternal <$> sckPlus1 <*> sck

-- TODO: 型制約の簡約化
exteriorDerivativeDualInternal ::
    forall n l c k a.
    (SingI l, KnownNat n, KnownNat k, VU.Unbox a, MSL.Numeric a, AdditiveGroup a, SingI c) =>
    Proxy k ->
    ExteriorDerivative n l (DualMap c) (DualDeg n (SuccDeg n k)) a ->
    ExteriorDerivative n l c k a
exteriorDerivativeDualInternal _ =
    case poincareDuality @n @l @c @(SuccDeg n k) of
        Refl ->
            case poincareDualitySucc @n @l @c @k of
                Refl -> case knownMatSizeDict @n @l @c @k @ExteriorDerivativeType of
                    (Dict, Dict) -> DECrepresentationMatrix . integralToSign (nInt - kInt) . MSG.transpose . unDECrepresentationMatrix
  where
    nInt = natToInt (Proxy :: Proxy n)
    kInt = natToInt (Proxy :: Proxy k)

exteriorDerivativeDualMat ::
    forall n l k a sig m.
    (SingI l, KnownNat n, KnownNat k, Algebra sig m, Has (Connectivity n l) sig m, AdditiveGroup a, VU.Unbox a, MSL.Numeric a) =>
    m (ExteriorDerivative n l Dual k a)
exteriorDerivativeDualMat =
    let proxyn = (Proxy :: Proxy n); proxyk = (Proxy :: Proxy k)
     in case succDegDict @n @k of
            Dict ->
                let proxysuccDegNK = (Proxy :: Proxy (SuccDeg n k))
                 in case dualSuccDegDict @n @k of
                        Dict ->
                            case knownMatSizeDict @n @l @Primal @(SuccDeg n k) @ExteriorDerivativeType of
                                (Dict, Dict) -> exteriorDerivativePrimalMat @n @l @(DualSuccDeg n k) >>= \d -> return $ exteriorDerivativeDualInternal proxyk d

-- exteriorDerivativeDualMat = exteriorDerivativePrimalMat @n @l @(DualSuccDeg n k) >>= \d -> return $ exteriorDerivativeDualInternal (Proxy :: Proxy k) d

exteriorDerivativeMat ::
    forall n l c k a sig m.
    (KnownNat k, KnownNat n, Algebra sig m, SingI l, SingI c, AdditiveGroup a, VU.Unbox a, MSL.Numeric a, Has (Connectivity n l) sig m) =>
    m (ExteriorDerivative n l c k a)
exteriorDerivativeMat = case (sing @c) of
    SPrimal -> exteriorDerivativePrimalMat
    SDual -> exteriorDerivativeDualMat
    SCEmpty -> return zero

exteriorDerivative ::
    forall n l c k a sig m.
    (HasExteriorDerivative n l c k a sig m) =>
    m (DifferentialForm n l c k a -> DifferentialForm n l c (SuccDeg n k) a)
-- exteriorDerivative = do
--   d <- exteriorDerivativeMat @n @l @c @k @_ @a
--   return $ \x -> d .@. x
exteriorDerivative =
    let proxyn = (Proxy :: Proxy n); proxyl = (Sing :: Sing l); proxyc = (Sing :: Sing c); proxyk = (Proxy :: Proxy k)
     in case knownMatSizeDict @n @l @c @k @ExteriorDerivativeType of
            (Dict, Dict) -> exteriorDerivativeMat @n @l @c @k @a >>= \d -> return (d .@.)

-- TODO: Simplicial Complex 1の数に抜けがないか確認する関数を作成
-- n-simplexに対してもsortしてindexを探しやすいようにする
-- degreeで場合分け？
-- 0-formとn-formの場合とその他など
-- 前者の計算は割と自明
-- wedge product

-- >>> import qualified Data.Set as S
-- >>> a = S.fromList [[0,1],[1,3],[0,3]]
-- >>> b = S.fromList [[1,2],[2,3],[1,3]]
-- >>> removeIntersection a b
-- fromList [[0,1],[0,3],[1,2],[2,3]]
removeIntersection x y = S.union x y `S.difference` S.intersection x y

-- >>> sc3 = Simplices $ S.fromList $ coerce @[[Int]] @[Simplex 3] [[0,1,2],[1,3,2],[2,3,4]]
-- >>> extractBoundaryElements sc3
-- Simplices (fromList [Simplex [0,1],Simplex [0,2],Simplex [1,3],Simplex [2,4],Simplex [3,4]])
extractBoundaryElements :: forall n l. (KnownNat n, 1 <= n) => Simplices n l -> BoundarySimplices (n - 1) l
extractBoundaryElements x = BoundarySimplices $ foldl' removeIntersection (S.fromList []) (S.map (S.map sortSimplex . removeIndexToSet) $ coerce x)

-- inclusionMapIndices :: forall n l. KnownNat n => Simplices n l -> BoundarySimplices n l -> Set Index
inclusionMapIndices (Simplices sc) (BoundarySimplices bs) = S.map (fromMaybe 0) $ S.map (adjacencySC sc) bs

indexToCOO :: Multiplicative a => Index -> COO a
indexToCOO i = (i, i, one)

type HasInclusionMapInternal n l c a =
    ( VU.Unbox a
    , VST.Storable a
    , KnownNat n
    , SingI l
    , SingI c
    , Multiplicative a
    , 1 <= n
    )

inclusionMapMatrixInternal ::
    forall n l c a.
    ( HasInclusionMapInternal n l c a
    ) =>
    Simplices n l ->
    InclusionMap n l c a
inclusionMapMatrixInternal sc = case toMatSizeFromTypeDict @n @l @c @(n - 1) @InclusionMapType of
    Dict -> DECrepresentationMatrix . MSS.fromTriplet . VU.map indexToCOO . fromList . toList $ S.map coerce $ inclusionMapIndices sc' be
  where
    sc' = generateSimplicialKComplexToKminus1 sc
    be = extractBoundaryElements sc

type HasInclusionMap n l c a sig m =
    ( MSL.Numeric a
    , HasInclusionMapInternal n l c a
    , Has (Connectivity n l) sig m
    )

inclusionMapMat ::
    forall n l c a sig m.
    (HasInclusionMap n l c a sig m) =>
    m (InclusionMap n l c a)
inclusionMapMat =
    case leqNat (Proxy :: Proxy 1) (Proxy :: Proxy n) of
        Just Refl -> do
            inclusionMapMatrixInternal <$> getPrimitiveSimplicies
        Nothing -> return zero

inclusionMap :: forall n l c a k sig m. (HasInclusionMap n l c a sig m) => m (DifferentialForm n l c (n -1) a -> DifferentialForm n l c (n -1) a)
-- inclusionMap = do
--   i <- inclusionMapMat @n @l @c @a
--   return $ \x -> i .@. x
inclusionMap = inclusionMapMat @n @l @c @a >>= \i -> return (i .@.)

-- TODO: wedge productの作成
wedge :: m (DifferentialForm n l c k1 a -> DifferentialForm n l c k2 a -> DifferentialForm n l c (k1 + k2) a)
wedge = undefined

eulerCharacteristics :: [Natural] -> Integer
eulerCharacteristics l = sum $ zipWith (*) a l'
  where
    a = map ((-1) ^) [0, 1 ..]
    n = length l
    l' = map fromIntegral l

-- type family EulerCharacteristics (l::SSizes) where
--   EulerCharacteristics '[] = 0
--   EulerCharacteristics (x ': xs) = 0