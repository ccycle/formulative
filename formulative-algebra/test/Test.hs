{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}

module Test where

import Formulative.Algebra.Prelude hiding (GAdditive)
import Formulative.Algebra.Vector
import GHC.Generics

literalNaturalTest :: Natural
literalNaturalTest = 1

-- This should throw compilation error
-- testFunc1 = -1 :: Natural

data Test1 = Test1 {a :: Double, b :: Double}
    deriving stock (Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace)

unit_addTest :: IO ()
unit_addTest = print $ Test1{a = 1, b = 0} + zero

unit_testFunc :: IO ()
unit_testFunc = print literalNaturalTest

unit_literal :: IO ()
unit_literal = let x = 2 :: Int in print (2 x :: Int)