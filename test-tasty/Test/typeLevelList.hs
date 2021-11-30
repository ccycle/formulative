{-# LANGUAGE UndecidableInstances #-}

-- import Data.Proxy
import Data.Singletons
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Num
import Data.Singletons.TypeLits

-- Proxy a -> Sing a

printNats :: forall (ns :: [Nat]). Sing ns -> IO ()
printNats ss = case ss of
    SNil ->
        return ()
    s `SCons` ss' -> do
        print $ fromSing s
        printNats ss'

printNatsTest = printNats (Sing :: Sing [1, 2, 3, 4])

type family (!) (l :: [Nat]) (n :: Nat) where
    '[] ! _ = 0
    (x ': _) ! 0 = x
    (_ ': xs) ! n = xs ! (n - 1)

-- >>> import GHC.Natural
-- >>> a1 = 2 :: Natural
-- >>> a2 = 5 :: Natural
-- >>> b = map fromIntegral [a1,a2] :: [Demote Nat]
-- >>> withSomeSing b printNats

nInput = 2 :: Demote Nat

printFromSing :: forall (n :: Nat). Sing n -> String
printFromSing proxy = show $ fromSing proxy

-- printFromProxy :: forall n. (KnownNat n) => Proxy n -> IO ()
-- printFromProxy proxy = print $ natVal proxy

-- >>> withSomeSing nInput printFromSing
-- "2"
