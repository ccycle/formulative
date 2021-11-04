{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- import Language.Haskell.Liquid.Prelude
-- import LambDEC.Calculation.Operator.Arithmetic.Class
-- import LambDEC.Calculation.Operator.VectorSpace.Class

{-@ type NonEmpty a = {v:[a]| 0 < len v} @-}
{-@ myHead::NonEmpty a -> a @-}
myHead (x : _) = x
myHead [] = error "Fear not! 'twill ne'er come to pass"

{-@ safeDiv :: (Eq a, Fractional a) => a -> { v: a | v /= 0 } -> a @-}
safeDiv n 0 = error "div 0"
safeDiv a b = a / b

conds n m l
    | n == 0 && m /= 0 = putStrLn "n=0"
    | m == 0 = putStrLn "m=0"
    | otherwise = putStrLn ("safeDiv: " ++ show (safeDiv n m)) >> putStrLn ("last input: " ++ show l)
main = do
    putStrLn "enter three numbers"
    putStrLn "1st:"
    (n :: Double) <- read <$> getLine
    putStrLn "2nd:"
    (m :: Double) <- read <$> getLine
    putStrLn "3rd:"
    (l :: Double) <- read <$> getLine
    conds n m l
