{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Dynamic.PDE where

import Control.Algebra
import Control.Carrier.Error.Church
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Sum
import Control.Exception.Safe
import Data.Csv (FromRecord, ToField, ToRecord)
import Data.Proxy
import Data.Singletons (Sing, SingInstance (SingInstance), SingKind (toSing), SomeSing (SomeSing), singInstance)
import Data.Singletons.Prelude (SingI)
import Data.Singletons.Prelude.List (SList)
import Dhall
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import OptDEC.Calculation.Algebra.Arithmetic.Class
import OptDEC.Calculation.DifferentialEquation.Parameter
import OptDEC.Calculation.DiscreteExteriorCalculus.Algebra
import OptDEC.Calculation.DiscreteExteriorCalculus.Class
import OptDEC.Calculation.Internal.Class
import OptDEC.Calculation.Internal.Singletons
import OptDEC.Calculation.Internal.TypeLevelNatural
import OptDEC.Calculation.Optimization.LineSearch
import OptDEC.Calculation.Optimization.Update
import OptDEC.Calculation.VectorSpace.Class
import OptDEC.Postprocess.Export.Class
import OptDEC.Preprocess.Label
import Test.Tasty

-- Cahn-Hilliard equation
data StateTest n l = MkStateTest {position :: VectorValuedDifferentialForm n n l Dual 0 Double, density :: DifferentialForm n l Dual 0 Double}
    deriving stock (Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
    deriving anyclass (ToRecord)

-- data SomeStateTest where
--     SomeStateTest :: Proxy (n :: Nat) -> SList (l :: [Nat]) -> StateTest n l -> SomeStateTest
-- deriving instance Show SomeStateTest

instance (Monad m, KnownNat n, SingI l) => HasInitialConditionM m (StateTest n l) where
    getInitialConditionM = return $ MkStateTest zero one
instance (Monad m, KnownNat n, SingI l) => HasUpdateM m (StateTest n l) where
    updateM = pure

-- instance (Monad m) => HasUpdateM m (SomeStateTest) where
--     updateM x = case x of
--         SomeStateTest (Proxy :: Proxy n) (sing :: SList l) y ->  updateM (y :: StateTest n l)

-- mainCalc :: forall n l sig m. (HasMainCalculationDynamic sig m (StateTest n l), Monad m, HasUpdateM m (StateTest n l), Member (Reader LabelsOfVariable) sig) => Proxy (n :: Nat) -> SList (l :: [Nat]) -> m ()
-- mainCalc _ _ =
--     mainCalculationDynamic @(StateTest n l)

data MyParameter = MyParameter {a1 :: Double, a2 :: Double}

-- instance (Has (Lift IO) sig m, KnownNat n, SingI l) => HasDynamicParameterSettingM m (StateTest n l) where
--     -- type ParameterType (StateTest n l) = Double
--     getDynamicParameterSettingM =
--         sendIO $ input auto "./setting.dhall"

-- instance Monad m => ExportSubDataM m (StateTest n l) where
--     exportSubDataM x = return ()

-- mainCalc' ::
--     ( Algebra sig m
--     , Member (Lift IO) sig
--     , Member (Throw SomeException) sig
--     , Member (Reader (DynamicParameterSetting Double)) sig
--     , Member (Reader LabelOfDynamicParameter) sig
--     , Member (Reader LabelsOfVariable) sig
--     , Member (Reader DimensionOfManifold) sig
--     , Member OutputDirEff sig
--     , Member LabelsOfQuantitiesEff sig
--     ) =>
--     m ()
-- mainCalc' = case (someNatVal 2, someSingVal [4 :: Natural, 5, 2]) of
--     (SomeNat pn, SomeSingI sl) -> mainCalc pn sl
-- mainCalc' = withSomeSingI [4, 5, 2] $ withSomeNat 2 mainCalc

stateTestInit = MkStateTest @2 @[4, 5, 2] zero zero

-- genStateTestInit :: forall (n :: Nat) (l :: [Nat]). (KnownNat n, SingI l) => Proxy n -> Sing l -> SomeStateTest
-- genStateTestInit pn pl = SomeStateTest pn pl (MkStateTest zero zero)
-- stateTestInit2 = withSomeSingI [4, 5, 2] $ withSomeNat 2 genStateTestInit

-- State n l -> m ()
-- Proxy n -> Proxy l -> m ()
printTest (MkStateTest a b) = print a >> print b
printTestProxy :: forall (n :: Nat) (l :: [Nat]). (KnownNat n, SingI l) => Proxy n -> Sing l -> IO ()
printTestProxy _ _ = printTest (MkStateTest @n @l zero one)
printTestSome = withSomeSingI [4, 5, 2] $ withSomeNat 2 printTestProxy

-- genStateTestInitM :: forall (n :: Nat) (l :: [Nat]) m. (Monad m, KnownNat n, SingI l) => Proxy n -> Sing l -> m SomeStateTest
-- genStateTestInitM pn pl = SomeStateTest pn pl <$> (getInitialConditionM)
-- stateTestInitM2 :: (Monad m) => m SomeStateTest
-- stateTestInitM2 = withSomeSingI [4, 5, 2] $ withSomeNat 2 genStateTestInitM
-- printStateTestInitM2 :: IO ()
-- printStateTestInitM2 = do
--     x <- (withSomeSingI [4, 5, 2] $ withSomeNat 2 genStateTestInitM)
--     print x