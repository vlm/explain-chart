> module ChartModel.Constraints where

> import Math.Polynomial
> import ChartModel.Shape
> import ChartModel.Geometry

Modules for debugging and testMinimize. Not strictly needed here.

> import Debug.Trace
> import Numeric.GSL.Minimization
> import Data.Packed.Matrix
> import Graphics.Plot

Given a polynome of degree D and a list of coordinate pairs which lie
on that polynome, we produce a cost function for minimizing the coefficients.

Compute cost given such constraints as a desired center of the shape
(which may differ between lines and parabolas), the list of intersections,
and the initial values or ranges for the coefficients.

> costFunction :: (Double, Double) -> [Coefficient Double] -> [(Double, Double)] -> (Int, [(String, [Double] -> Double)])
> costFunction (center_x, center_y) coeffs coords =
>   let individual_coeff_constraints = map coeffConstraint coeffs
>       -- Compute the cost of changing the coefficients
>       cost1 cs = sum (zipWith id individual_coeff_constraints cs)
>       -- Compute the cost of intersection with coords
>       cost2 cs = sum (map (intersection_constraint cs) coords)
>       -- ax+b=y, cs=[b,a], x and y are known. Minimize (y-(ax+b))^n.
>       intersection_constraint cs (x, y) = (y - evalPoly (poly LE cs) x) ** 2
>       -- If no other constraints are in place, the graphs are centered.
>       cost3 cs = (sigmcost (center_y - evalPoly (poly LE cs) center_x)) / 1E20
>   in (length coeffs, [("change coeffs", cost1), ("intersection", cost2), ("centering", cost3)])

A coefficient cost function for minimizing coefficients. Given a coefficient
description (Coefficient) and a coefficient candidate, checks whether candiate
matches the constraints well. The function is not binary: to be admissible to
the optimizer we must to be able to compute its derivative.

> coeffConstraint CoeffAny            k = 0
> coeffConstraint (CoeffExact a)      k = (10 * (a-k))**10
> coeffConstraint cf@(CoeffRange lin (l, r)) k
>   | signum l * signum r < 0 = error (show cf ++ ": same sign expected")
>   | l > r = coeffConstraint (CoeffRange lin (r, l)) k
>   | signum l < 0 = coeffConstraint (CoeffRange lin (-l, -r)) (-k)
>   | signum k < 0 = coeffConstraint (CoeffRange lin (r - k, r - k)) 0
>   | lin == Linear =
>       let avg = average (l, r)
>       in case ((k - avg) / avg) ** 10 of
>           c | c < 0.5 -> sigmcost c
>             | otherwise -> c
>   | lin == NonLinear =
>       let lavg = log_average (l, r)
>       in case exp (log k - lavg) - 1 of
>           c -> sigmcost c

> -- http://en.wikipedia.org/wiki/Sigmoid_function
> -- sigm is a function which looks like right side
> -- of sigmoid replicated to the left and with sigmcost 0 = 0.
> sigmcost x = (1 / (1 + exp (- abs x)) - 0.5)

Usage:

$ ghci ChartModel/Constraints.lhs
ghci> testMinimize (10, 10) [CoeffAny, CoeffRange (-1,-2)] [] [20,20] [1,1]

> testMinimize :: (Double, Double) -> [Coefficient Double] -> [(Double, Double)] -> [Double] -> [Double] -> IO [Double]
> testMinimize center coeffs coords box init_coeffs =
>   let (degree, cost_functions) = costFunction center coeffs coords 
>       cost_f cs = sum $ map (flip snd cs) cost_functions
>       (min, p) = minimize NMSimplex2 1E-5 100
>                       box cost_f init_coeffs
>   in
>   do
>       mplot (drop 3 (toColumns p))
>       return min


