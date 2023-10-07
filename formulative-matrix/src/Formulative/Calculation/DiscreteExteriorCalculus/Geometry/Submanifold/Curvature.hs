module Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Submanifold.Curvature where

import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.DifferentialEquation.Dynamics.Class
import Formulative.Calculation.VectorSpace.InnerProductSpace

-- dX/dt = f(X)
-- Reference:
-- E. N. Lages, E. S. S. Silveira, D. T. Cintra, and A. C. Frery, “An adaptive time integration strategy based on displacement history curvature,” Int. J. Numer. Meth. Engng, vol. 93, no. 12, pp. 1235–1254, Mar. 2013, doi: 10.1002/nme.4421.
curvatureValue f1 f2 x =
    let g1 = f1 x <.> f2 x
        g2 = f2 x <.> f2 x
        g3 = f1 x <.> f2 x
     in sqrt ((g1 * g2 .-. g3 .^ 2) ./. g1 .^ 3)
