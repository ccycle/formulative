{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- import Language.Haskell.Liquid.Prelude
-- import LambDEC.Calculation.Operator.Arithmetic.Class
-- import LambDEC.Calculation.Operator.VectorSpace.Class

{-@ type NonEmpty a = {v:[a]| 0 < len v} @-}
{-@ myHead::NonEmpty a -> a @-}
myHead (x : _) = x
myHead [] = error "Fear not! 'twill ne'er come to pass"

{-@ safeDiv :: Int -> { d:Int | d /= 0 } -> Int @-}
safeDiv :: Int -> Int -> Int
safeDiv n 0 = error "zero divide"
safeDiv n d = n `div` d

{-@ safeDiv2 :: (Eq a, Fractional a) => a -> { v: a | v /= 0 } -> a @-}
safeDiv2 n 0 = error "div 0"
safeDiv2 a b = a / b

conds n m l
    | n == 0 && m /= 0 = putStrLn "n=0"
    | m == 0 = putStrLn "m=0"
    | otherwise = putStrLn ("safeDiv: " ++ show (safeDiv2 n m)) >> putStrLn ("last input: " ++ show l)
main = do
    -- print $ safeDiv 2 1
    -- print $ safeDiv2 1 1
    -- print $ safeDiv2 0 1
    putStrLn "enter three numbers"
    putStrLn "1st:"
    (n :: Double) <- read <$> getLine
    putStrLn "2nd:"
    (m :: Double) <- read <$> getLine
    putStrLn "3rd:"
    (l :: Double) <- read <$> getLine
    conds n m l
