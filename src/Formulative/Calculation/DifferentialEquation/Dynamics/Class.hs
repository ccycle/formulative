module Formulative.Calculation.DifferentialEquation.Dynamics.Class where

import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.VectorSpace.Class
import Prelude hiding (Floating (..))

-- dx/dt = f(x)
-- f: evolutionFunctionM
-- f': evolutionFunctionDerivM
class (Monad m) => HasEvolutionFunction m a where
    evolutionFunctionM :: m (a -> a)

-- class (Monad m) => HasEvolutionFunctionDeriv m a where
--     evolutionFunctionDerivM :: m (a -> a)

-- Reference:
-- E. N. Lages, E. S. S. Silveira, D. T. Cintra, and A. C. Frery, “An adaptive time integration strategy based on displacement history curvature,” Int. J. Numer. Meth. Engng, vol. 93, no. 12, pp. 1235–1254, Mar. 2013, doi: 10.1002/nme.4421.
curvatureValueSpaceTime f1 x =
    let f2 = f1 . f1
        g1 = f1 x <.> f2 x
        g2 = f2 x <.> f2 x
        g3 = f1 x <.> f2 x
     in sqrt' (((one .+. g1) .*. g2 .-. g3 .^ 2) ./. (one .+. g1) .^ 3)
curvatureValueSpaceTimeM x = do
    f1 <- evolutionFunctionM
    return $ curvatureValueSpaceTime f1 x
