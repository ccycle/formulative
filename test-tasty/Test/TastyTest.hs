module Test.TastyTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

unit_print :: IO ()
unit_print = putStrLn "print test"

prop_equal :: Int -> Bool
prop_equal a = a == a