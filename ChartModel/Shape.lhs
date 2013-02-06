> {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

> module ChartModel.Shape (Shape(..),
>                          PolyShape(..),
>                          Coefficient(..),
>                          parseAnyShape
>                          ) where

> import Data.List
> import Math.Polynomial

> import ChartModel.Parser
> import ChartModel.SpecialPoint

> data Coefficient a = CoeffExact a | CoeffRange (a, a) | CoeffAny
>                      deriving Eq

Shape is a collection of regular drawing primitives: lines, curves, etc.

> class PolyShape a where
>    coefficients :: a -> [Coefficient Double] -- Little-Endian, e.g. b+ax+cx^2

> data Shape = forall a. (Show a, PolyShape a) => Shape {
>               name :: String,
>               shape :: a,
>               shape_intersections :: [SpecialPoint]
>              }
> instance Show (Shape) where
>   show (Shape n s i) =
>       "Shape { " ++ intercalate "," [show n, show s, show i] ++ " }"

There are many kinds of shapes. We don't know how to parse neither of them.
So we pick a list of all possible PolyShape-compatible parsers and try them
in turn, returning the complete Shape.

> parseAnyShape primitive_shapes = do
>   name <- identifier
>   reservedOp "="
>   prim <- choice primitive_shapes
>   return (Shape name prim [])


Given a polynome of degree D and a list of coordinate pairs which lie
on that polynome, we produce a cost function for minimizing the coefficients.

 let f = snd (constraints (10,10) [CoeffAny, CoeffRange (2,4)] [(5,15)])
 let cs = fst $ minimize NMSimplex2 1E-5 1000 [100,100] f [1,1]
 let p = evalPoly (poly LE cs)
 let range = [0..20.0] in mplot $ [fromList $ range, fromList $ map p range]

or

let f r c i = let f = snd (constraints r c i) in let cs = fst $ minimize NMSimplex2 1E-5 1000 [100,100] f [1,1] in let p = evalPoly (poly LE cs) in let range = [0..20.0] in mplot $ [fromList $ range, fromList $ map p range]

f (10,10) [CoeffExact 10, CoeffRange (0.5,1.5)] []

> constraints :: (Double, Double) -> [Coefficient Double] -> [(Double, Double)] -> (Int, [Double] -> Double)
> constraints (center_x, center_y) coeffs coords =
>   let individual_coeff_constraints = map coeffConstraint coeffs
>       -- Compute the cost of changing the coefficients
>       cost1 cs = sum (zipWith id individual_coeff_constraints cs)
>       -- Compute the cost of intersection with coords
>       cost2 cs = sum (map (intersection_constraint cs) coords)
>       -- ax+b=y, cs=[b,a], x and y are known. Minimize (y-(ax+b))^n.
>       intersection_constraint cs (x, y) = (y - evalPoly (poly LE cs) x) ** 10
>       -- If no other constraints are in place, the graphs are centered.
>       cost3 cs = (sigmcost $ center_y - evalPoly (poly LE cs) center_x)
>   in (length coeffs, \cs -> cost1 cs + cost2 cs + cost3 cs)
>   where

A coefficient cost function for minimizing coefficients. Given a coefficient
description (Coefficient) and a coefficient candidate, checks whether candiate
matches the constraints well. The function is not binary: to be admissible to
the optimizer we must to be able to compute its derivative.

>       coeffConstraint CoeffAny            k = 0
>       coeffConstraint (CoeffExact a)      k = (10 * (a-k))**10
>       coeffConstraint (CoeffRange (l, r)) k
>           | l < r =
>               let avg = (r-l)/2
>                   pow = 10
>               in (k - (l + avg)) ** pow / (avg ** pow)
>           | otherwise = coeffConstraint (CoeffRange (r, l)) k

> -- http://en.wikipedia.org/wiki/Sigmoid_function
> -- sigm is a function which looks like right side
> -- of sigmoid replicated to the left and with sigmcost 0 = 0.
> sigmcost x = (1 / (1 + exp (- abs x)) - 0.5)

