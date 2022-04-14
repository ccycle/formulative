{-# LANGUAGE OverloadedStrings #-}

module Formulative.Preprocess.Exception where

import Control.Arrow
import Control.Carrier.Error.Church
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable (Proxy, TypeRep, typeOf, typeRep)
import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Algebra.Arithmetic.Additive
import GHC.Generics
import GHC.TypeNats
import Refined
import Text.Read

maybeToMonadThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeToMonadThrow e = throwM e `maybe` return

eitherToMonadThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToMonadThrow = throw ||| return

-- instance Exception PathException

-- safe read
data ReadException = ReadException String TypeRep
    deriving (Show, Typeable)

instance Exception ReadException where
    displayException (ReadException a b) =
        concat
            [ "*** ReadException: The input value does not match the expected type."
            , "\n"
            , "input: "
            , a
            , "\n"
            , "expected type: "
            , show b
            ]

readM :: (MonadThrow m, Read a, Typeable a) => String -> m a
readM s = res
  where
    res = maybeToMonadThrow (ReadException s (typeRep res)) (readMaybe s)

-- >>> :set -XTypeApplications
-- >>> readM "1.0" :: IO Double
-- >>> readM @IO @Double "1.0"
-- Expected kind ‘* -> *’, but ‘Double’ has kind ‘*’
-- >>> readM @IO Int "1.0"
-- Expected kind ‘* -> *’, but ‘Int’ has kind ‘*’

-- safe fromList
data FromListException = FromListException deriving (Show)
instance Exception FromListException

-- >>> :set -XDataKinds
-- >>> import Data.Proxy
-- >>> v = fromListToSizedVector (Proxy :: Proxy 3) [1,2,3 :: Int]
-- >>> v
-- Vector [1,2,3]
-- >>> :t v
-- v :: MonadThrow m => m (Vector 3 Int)
-- >>> vFail = fromListToSizedVector (Proxy :: Proxy 4) [1,2,3 :: Int]
-- >>> vFail
-- FromListException
fromListToSizedVector :: forall n a m. (KnownNat n, MonadThrow m) => Proxy n -> [a] -> m (VS.Vector n a)
fromListToSizedVector _ l = maybeToMonadThrow FromListException (VS.fromList l)

data MatrixSolveException = MatrixSolveException deriving (Show)
instance Exception MatrixSolveException

class (RealFloat a) => IsFiniteM m a where
    anyM :: (a -> Bool) -> m a -> Bool
    isFiniteM :: m a -> Bool
    isFiniteM = anyM (\x -> isNaN x || isInfinite x)

data AllLessThan (n :: Nat) = AllLessThan deriving (Generic)
instance (Ord a, Num a, KnownNat n, Foldable t) => Predicate (AllLessThan n) (t a) where
    validate p x = do
        let n = natVal (Proxy :: Proxy n)
        let x' = fromIntegral n
        if all (< x') x
            then Nothing
            else
                throwRefineOtherException
                    (typeRep p)
                    ("Not all values are less than " <> (T.pack . show) n)

newtype ConvergenceException a = ConvergenceException a deriving (Show)
instance (Show a, Typeable a) => Exception (ConvergenceException a) where
    displayException (ConvergenceException x) = concat ["*** ConvergenceException: the value does not converge.", "\n", "residual: ", show x]

printSomeException :: (Algebra sig m, Member (Lift IO) sig) => SomeException -> m ()
printSomeException e = sendIO $ putStrLn "" >> (putStrLn . displayException $ e)

runSomeException j = runError @SomeException j return