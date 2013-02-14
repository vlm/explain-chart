> module ChartModel.Constraints (costFunction) where

> import Math.Polynomial
> import ChartModel.Shape

Given a polynome of degree D and a list of coordinate pairs which lie
on that polynome, we produce a cost function for minimizing the coefficients.

 let (l,f) = costFunction (10,10) [CoeffAny, CoeffRange (2,4)] [(5,15)]
 let cs = fst $ minimize NMSimplex2 1E-5 1000 (replicate l 10) f (replicate l 1)
 let p = evalPoly (poly LE cs)
 let range = [0..20.0] in mplot $ [fromList $ range, fromList $ map p range]

or

let f r c i = let (l,f) = costFunction r c i in
let cs = fst $ minimize NMSimplex2 1E-5 10000 (replicate l 10) f (replicate l 1) in
let p = evalPoly (poly LE cs) in let range = [0..20.0] in do { mplot $ [fromList $ range, fromList $ map p range]; print cs; print (fst r, p $ snd r); print (map (p . fst) i) }


f (10,10) [CoeffExact 10, CoeffRange (0.5,1.5)] []

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
>       intersection_constraint cs (x, y) = abs (y - evalPoly (poly LE cs) x)
>       -- If no other constraints are in place, the graphs are centered.
>       cost3 cs = (sigmcost $ center_y - evalPoly (poly LE cs) center_x) / 1E20
>   in (length coeffs, [("change coeffs", cost1), ("intersection", cost2), ("centering", cost3)])
>   where

A coefficient cost function for minimizing coefficients. Given a coefficient
description (Coefficient) and a coefficient candidate, checks whether candiate
matches the constraints well. The function is not binary: to be admissible to
the optimizer we must to be able to compute its derivative.

>       coeffConstraint CoeffAny            k = 0
>       coeffConstraint (CoeffExact a)      k = (10 * (a-k))**10
>       coeffConstraint (CoeffRange (l, r)) k
>           | l < r =
>               let avg = l + (r-l)/2
>                   pow = 10
>               in case ((k - avg) / avg) ** pow of
>                    c | c < 0.5 -> sigmcost c
>                      | otherwise -> c
>           | otherwise = coeffConstraint (CoeffRange (r, l)) k

> -- http://en.wikipedia.org/wiki/Sigmoid_function
> -- sigm is a function which looks like right side
> -- of sigmoid replicated to the left and with sigmcost 0 = 0.
> sigmcost x = (1 / (1 + exp (- abs x)) - 0.5)

