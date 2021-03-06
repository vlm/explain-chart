> {-# LANGUAGE ViewPatterns #-}
> module ChartModel.Constraints where

> import Data.List
> import Data.Maybe
> import Math.Polynomial
> 
> import ChartModel.Shape
> import ChartModel.Expression
> import ChartModel.Geometry

Modules for debugging and testMinimize. Not strictly needed here.

> import Debug.Trace
> import Numeric.GSL.Minimization
> import Data.Packed.Matrix
> import Numeric.Container hiding (find)
> import Graphics.Plot

A coefficient cost function for minimizing individual coefficients.
Given a coefficient description (Coefficient) and a coefficient candidate,
checks whether candiate matches the constraints well.
The function is not binary: to be admissible to the optimizer we ought
to be able to compute its derivative.

> coeffConstraint CoeffAny            k = 0
> coeffConstraint (CoeffExact a)      k = (10 * (1 + abs (a-k))) ** 2
> coeffConstraint cf@(normalizeCoefficient -> CoeffRange lin (l, r)) k
>   | signum l * signum r < 0 = error (show cf ++ ": same sign expected")
>   | signum l < 0 = coeffConstraint (CoeffRange lin (-l, -r)) (-k)
>   | lin == Linear =
>       let avg = average (l, r)
>           delta = (r - l) / 2
>       in case (abs (k - avg)) / delta of
>           c | c < 1 -> c ** 2
>             | otherwise -> c ** 10
>   | lin == NonLinear =
>       let lavg = log_average (l, r)
>       in case abs (k - lavg) of
>           c | k < l -> (10 + abs (k - l)) ** 10
>           c | k > r -> (10 + abs (k - r)) ** 10
>           c -> (1 + exp c) ** 2


There is a slew of functions that take polynomial coefficients and return
some sort of result based on them. PolyCompute is a type for such functions.

> type PolyCompute = SinglePolynome -> Double
> type SinglePolynome = [Double]    -- A set of coefficients for a single poly.
> data AllPolynomes = AP [Double]   -- A global list of all coefficients

The CostStructure defines how to compute various parts of the
objective cost function based on given set of polynomial coefficients.

> data CostStructure a = CostParts {
>   cp_label         :: String, -- A label for debugging purposes
>   cp_change_coeffs :: a,  -- Cost of changed coefficients
>   cp_intersection  :: a,  -- Cost of deviating from intersection
>   cp_centering     :: a,  -- Cost of moving away from the center
>   cp_extremum      :: a   -- Cost of moving parabola extremum off center
>  } deriving (Show)
> instance Functor CostStructure where
>   fmap f (CostParts s a b c d) =
>           CostParts s (f a) (f b) (f c) (f d)

Extract the individual cost constituents in a list form.

> cost_parts :: CostStructure a -> [a]
> cost_parts (CostParts _ a b c d) = [a, b, c, d]

The total cost is computed as a simple sum of individual cost constituents.
The return value of polyCostFunction can be used as a first argument to sumCost.

> sumCost :: (a -> Double) -> CostStructure a -> Double
> sumCost f = sum . cost_parts . fmap f

Given a polynome of degree D and a list of coordinate pairs which lie
on that polynome, we produce a cost function for minimizing the coefficients.

Compute cost given such constraints as a desired center of the shape
(which may differ between lines and parabolas), the list of intersections,
and the initial values or ranges for the coefficients.

> polyCostFunction :: (Double, Double) -> [Coefficient Double] -> [(Double, Double)] -> CostStructure PolyCompute
> polyCostFunction (center_x, center_y) coeffs coords =
>   let individual_coeff_constraints = map coeffConstraint coeffs
>       -- Compute the cost of changing the coefficients
>       cost1 cs = sum (zipWith id individual_coeff_constraints cs)
>       -- Compute the cost of intersection with coords
>       cost2 cs = sum (map (intersection_constraint cs) coords)
>       -- ax+b=y, cs=[b,a], x and y are known. Minimize (y-(ax+b))^n.
>       intersection_constraint cs (x, y) = (1 + abs (y - evalPoly (poly LE cs) x)) ** 10
>       -- If no other constraints are in place, the graphs are centered.
>       cost3 cs =
>           let y = evalPoly (poly LE cs) center_x in
>           (1 + abs (y - center_y)) ** 2
>       -- (deriv^k) makes sure we center near the [parabola] extremum
>       cost4 cs = if length cs /= 3 then 0 else
>           let (_, deriv) = evalPolyDeriv (poly LE cs) center_x in
>           (1 + abs deriv) ** 2
>   in CostParts "polyCost" cost1 cost2 cost3 cost4



> exprCostFunction :: (Double, Double) -> (String -> Double -> Double) -> Expression -> [(Double, Double)] -> CostStructure Double
> exprCostFunction (center_x, center_y) name2f expr coords =
>   let
>       -- Compute the cost of intersection with coords
>       cost2 = sum $ map intersection_constraint coords
>       intersection_constraint (x, y) = (10 + abs (y - evalExpr name2f expr x)) ** 10
>       -- If no other constraints are in place, the graphs are centered.
>       cost3 =
>           let y = evalExpr name2f expr center_x in
>           (abs (y - center_y))
>   in CostParts {
>       cp_label = "expression cost " ++ showExpression expr,
>       cp_change_coeffs = 0,
>       cp_intersection = cost2,
>       cp_centering = cost3,
>       cp_extremum = 0
>        }

> -- http://en.wikipedia.org/wiki/Sigmoid_function
> -- sigm is a function which looks like right side
> -- of sigmoid replicated to the left and with sigmcost 0 = 0.
> sigmcost x = (1 / (1 + exp (- abs x)) - 0.5)

> plotPCF cfs =
>   let pcf = polyCostFunction (50, 50) cfs []
>   in mesh $ build (100, 100) (\i j ->
>       let a = i - 50
>           b = 0 -- (j - 50) / 10
>       in (cp_change_coeffs pcf) [a, b] + (cp_centering pcf) [a, b])

Usage:

$ ghci ChartModel/Constraints.lhs
ghci> testMinimize (10, 10) [CoeffAny, CoeffRange (-1,-2)] [] [20,20] [1,1]

> testMinimize :: (Double, Double) -> [Coefficient Double] -> [(Double, Double)] -> [Double] -> [Double] -> IO [Double]
> testMinimize center coeffs coords box init_coeffs =
>   let cost_functions = polyCostFunction center coeffs coords 
>       cost_f cs = sumCost ($ cs) cost_functions
>       (min, p) = minimize NMSimplex2 1E-10 1000
>                       box cost_f init_coeffs
>   in
>   do
>       mplot (drop 3 (toColumns p))
>       return min

Imagine the shapes are topologically sorted. That is, they ordered like
[A = B + C,  B = line,  C = D + 3,  D = parabola]. Each polynomial shape
(B and D) requires a few coefficients for their monomials (2 and 3
for line B and parabola D). Each derived shape (A and C) does not require
any coefficients, since they are simple translation of their dependencies.

We want to create a global optimization function which will receive
a long list of coefficients (5 for B and D), know which coefficients
correspond to which shape, and will return a combined cost function
for all shapes at once by splitting the list of coefficients and feeding
into cost functions for individual shapes.

Given a list of shapes we return a list of shapes zipped with the corresponding
positions and the subset lengths in the hypothetical coefficient list:
    [(A, (5,0)), (B (3,2)), (C, (3,0)), (D, (0,3)]

this can be interpreted as:

    A = ...       B = ex + d      C = ...       D = cx^2 + bx + a

where coeeficients [e,d,c,b,a] are represented by the coefficient
positions in that list:

    A = []        B = [4,3]       C = []        D = [2,1,0]

First we try figure out how many coefficients each shape takes. This is
simple: we just take advantage of the PolyForm being in a Polynomial typeclass
with coefficients just giving us the correctly sized list of necessary
coefficients. The DerivedForm does not take any coefficients.

> needsCoefficients (shape -> DerivedForm _) = 0
> needsCoefficients (shape -> PolyForm p) =
>   length (coefficients p undefined undefined)

Then we create a folding function which takes the last allocated position
in the list and reserves the necessary numbers of slots right after it.

> coefficientSpan s (currentPosition, acc) =
>   let needSlots = needsCoefficients s in
>   (currentPosition + needSlots,  ((s, (currentPosition, needSlots)) : acc))

Now given the list of shapes we return the desired structure. The first
member of a tuple is the number of coefficients required, and the rest
is a list of shapes and their cuts in the coefficient list.

> coefficientPositions :: [Shape] -> (Int, [(Shape, (Int, Int))])
> coefficientPositions = foldr coefficientSpan (0, []) . topSortShapes

Given a shape and the start position in the global list of coefficients,
returns the cost function which can compute a cost based on the
global list of coefficients for the whole scene.

Since there are many parameters, let's make it a helper structure.

> data ShapeCostArg = SCA {
>       sca_xrange :: XRange,
>       sca_yrange :: YRange,
>       -- Resolve a shape name into a start position in the
>       -- global coefficients list.
>       sca_span :: String -> (Int, Int),
>       sca_resolve :: String -> Double -> Double
>   }

And the function itself. Note that its return value differs from
polyCostFunction's return value in that the returns a function
that accepts the list of specific coefficients, whereas shapeCost
awaits the global list of coefficients [...,e,d,c,b,a]

*Main> shapeCost (SCA (0,100) (0, 100) (const (0, 2)) undefined) (Shape "L" (PolyForm $ PolyWrap $ InformalLine ChartModel.Line.Positive) []) [1,0]

> shapeCost :: ShapeCostArg -> Shape -> AllPolynomes -> CostStructure Double
> shapeCost (SCA xrange yrange spanOf resolve) s@(shape -> PolyForm p) (AP acs) =
>   let cx = center_x s xrange
>       cy = center_y s yrange
>       initial_cs = coefficients p xrange yrange
>       -- Cut out our 2 or 3 coefficients out of the long list of coeffs
>       (start, span) = spanOf (name s)
>       select = reverse . take span . drop start . reverse
>       ics = select acs    -- Get individual coefficients by start/span
>       cfs = polyCostFunction (cx, cy) initial_cs
>                                       (map sp_xy $ shape_intersections s)
>   in fmap ($ ics) cfs
> shapeCost (SCA xrange yrange spanOf resolve) s@(shape -> DerivedForm expr) (AP _) =
>   let cx = center_x s xrange
>       cy = center_y s yrange in
>   exprCostFunction (cx, cy) resolve expr (map sp_xy $ shape_intersections s)

> evalShape evalByName cs (shape -> PolyForm p) = evalPoly (poly LE cs)
> evalShape evalByName cs (shape -> DerivedForm expr) = evalExpr evalByName expr

The shapesCost takes all shapes and creates a multi-coefficient function
which represent the optimization cost function jointly for all shapes.

> combinedShapesCost :: XRange -> YRange -> [Shape] -> (Int, [(Shape, (Int, Int))], AllPolynomes -> [CostStructure Double], AllPolynomes -> String -> Double -> Double)
> combinedShapesCost xrange yrange shapes =
>   let (len, positions) = coefficientPositions shapes
>       name2shape_and_span n = fromJust $ find ((n ==) . name . fst) positions
>       name2span = snd . name2shape_and_span
>       name2eval acs@(AP all_coefficients) n =
>           let (s', span) = name2shape_and_span n
>               cs = select span all_coefficients
>           in evalShape (name2eval acs) cs s'
>       name2scost n = snd . fromJust . flip lookup n
>       shape2cost acs (s, (start, slots)) acc =
>           let
>               costArg = SCA xrange yrange name2span (name2eval acs)
>           in (name s, shapeCost costArg s acs) : acc
>       costfs acs = foldr (shape2cost acs) [] positions
>       acs2cost acs = map snd (costfs acs)
>   in (len, positions, acs2cost, name2eval)
>   where select (start, span) = reverse . take span . drop start . reverse


