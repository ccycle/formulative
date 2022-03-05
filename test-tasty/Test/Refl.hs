{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Refl where

import Data.Proxy
import Data.Singletons
import Data.Type.Equality
import GHC.Natural
import GHC.TypeNats
import OptDEC.Calculation.DiscreteExteriorCalculus.Class
import OptDEC.Calculation.Internal.TypeLevelList
import OptDEC.Calculation.Internal.TypeLevelNatural
import Unsafe.Coerce

constrainedFunctionTest :: forall n. (1 <= n, KnownNat n) => Proxy n -> Natural
constrainedFunctionTest = natVal
notConstrainedFunctionTest :: forall n. (KnownNat n) => Proxy n -> Natural
-- notConstrainedFunctionTest = constrainedFunctionTest -- <- compile error
notConstrainedFunctionTest proxy = case leqNat (Proxy :: Proxy 1) proxy of
    Just Refl -> constrainedFunctionTest proxy
    Nothing -> natVal proxy

data TestCellData (c :: CellType) = TestCellData deriving (Show, Eq)

printCellType :: forall (c :: CellType). (SingI c) => SCellType c -> String
printCellType singc = case sameCellType SPrimal (Sing :: SCellType c) of
    Just Refl -> printPrimal (TestCellData :: TestCellData c) -- c ~ 'Primal
    Nothing -> "not Primal"

printPrimal :: TestCellData Primal -> String
printPrimal _ = show Primal

-- withSomeCell x = toSing x

-- singConstraint :: (forall (a :: k). Sing a -> r) -> (forall (a' :: k). (SingI a') => Sing a' -> r)
-- singConstraint = id

-- printCellTypeFromSing :: forall c. SCellType c -> String
-- printCellTypeFromSing singc = printCellType (TestCellData :: TestCellData c)

-- printTestPrimal = withSomeSing Primal printCellTypeFromSing