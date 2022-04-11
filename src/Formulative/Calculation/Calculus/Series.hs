module Formulative.Calculation.Calculus.Series where

import Data.Maybe
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Numeric.AD.Rank1.Tower
import RIO.List

headN list = fromMaybe zero (headMaybe list)
tailN list = fromMaybe [] (tailMaybe list)

-- f, f'/1! , f''/2! , f^(3)/3!, ..., f^n/n!
taylorSeriesCoefficients n f a x = take (n + 1) s where s = taylor f a x

-- f'/1! , f''/2! , f^(3)/3!, ... , f^n/n!
taylorSeriesCoefficients1 n f a x = take n $ tailN s where s = taylor f a x

-- 1, (x-a), (x-a)^2, ... , (x-a)^n
polynomials n a x = map ((x .-. a) .^) [0, 1 .. n]

-- (x-a), (x-a)^2, (x-a)^3 ... (x-a)^n
polynomials1 n a x = map ((x .-. a) .^) [1, 2 .. n]

-- (f(x) - f(a))/(x-a) = f'(a) + f''(a)(x-a)/2! + f'''(a)(x-a)^2/3! + ...
-- discreteVariation n f a x = g n (polynomials1 n a x) (head $ taylorSeriesCoefficients1 n f a x)
--     where
--         g n x polyList coeffsList |  []==coeffsList = x
--               |
--     -- let coeffs = taylorSeriesCoefficients n f a x
--     -- let poly = polynomials n a x

--     -- f'(a)
--     v <- newSTRef $ headN $ taylorSeriesCoefficients1 n f a x
--     polyST <- newSTRef $ polynomials1 n a x
--     coeffsST <- newSTRef $ taylorSeriesCoefficients1 n f a x

--     v <- newSTRef poly
--     forM_ [1 .. n] $ \i ->
--         modifySTRef v (.+. v)
--     readSTRef v
