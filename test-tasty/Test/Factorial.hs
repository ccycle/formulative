module Test.Factorial where

import Control.Monad
import Control.Monad.ST
import Data.STRef.Strict
import Math.Combinatorics.Exact.Factorial (factorial)

factorialList 0 = 1
factorialList n
    | n < 0 = 0
    | otherwise = product [1 .. n]

factorialRecursive 0 = 1
factorialRecursive n
    | n < 0 = 0
    | otherwise = n * factorialRecursive (n - 1)

factorialSTRef n
    | n < 0 = 0
    | otherwise = runST $ do
        x <- newSTRef 1
        forM_ [1 .. n] $ \i ->
            modifySTRef x (* i)
        readSTRef x
