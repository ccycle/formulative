{-# LANGUAGE UndecidableInstances #-}

module Test.SingletonTest where

import Data.Singletons
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Num
import Data.Singletons.TypeLits
import Test.Tasty

-- Proxy a -> Sing a

printNats :: forall (ns :: [Nat]). Sing ns -> IO ()
printNats ss = case ss of
    SNil ->
        return ()
    s `SCons` ss' -> do
        print $ fromSing s
        printNats ss'

unit_printNatsTest = printNats (Sing :: Sing [1, 2, 3, 4])

-- >>> import GHC.Natural
-- >>> a1 = 2 :: Natural
-- >>> a2 = 5 :: Natural
-- >>> b = map fromIntegral [a1,a2] :: [Demote Nat]
-- >>> withSomeSing b printNats

nInput = 2 :: Demote Nat

printFromSing :: forall (n :: Nat). Sing n -> IO ()
printFromSing proxy = print $ fromSing proxy

printFromProxy :: forall n. (KnownNat n) => Proxy n -> IO ()
printFromProxy proxy = print $ natVal proxy

-- >>> withSomeSing nInput printFromSing
-- "2"

unit_sing = withSomeSing nInput printFromSing
