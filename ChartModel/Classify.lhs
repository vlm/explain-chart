
Analyze and classify the relative location of the shapes (below, above, etc),
their intersections, etc.

> {-# LANGUAGE ViewPatterns #-}
> module ChartModel.Classify where
> import Data.List
> import Data.Complex
> import Math.Polynomial
> import Numeric.GSL.Polynomials
> import ChartModel.Geometry
> import ChartModel.Line

Find the list of intersections of the given two polynomes.

> intersections :: [Double] -> [Double] -> [(Double, Double)]
> intersections (poly LE -> poly1) (poly LE -> poly2) =
>   map (\x -> (x, evalPoly poly1 x))
>   $ map realPart $ filter ((== 0) . imagPart)
>   $ (\cfs -> if length cfs > 1 then polySolve cfs else [])
>   $ polyCoeffs LE $ sumPolys [poly1, negatePoly poly2]

Approximate the list of intersections within the given epsilon.

> approx :: Double -> [(Double, Double)] -> [(Double, Double)]
> approx epsilon =
>   nub . sort . map (\(x, y) -> (coarse x, coarse y))
>   where coarse x = (fromIntegral $ round (x / epsilon)) * epsilon

Discover the location of the first polynomial relative to the second.

> data RelativeLocation = Overlap [(Double, Double)]
>                       | RLeft | RRight | RAbove | RBelow deriving (Show)

> relativeLoc :: XRange -> YRange -> [Double] -> [Double] -> RelativeLocation
> relativeLoc xrange yrange poly1 poly2 =
>   case capbox xrange yrange $ approx 0.01 $ intersections poly1 poly2 of
>       [ ] | y_of poly1 > y_of poly2 && slope poly1 == SteepPositive -> RLeft
>           | y_of poly1 > y_of poly2 && slope poly1 == SteepNegative -> RRight
>           | y_of poly1 > y_of poly2 -> RAbove
>           | y_of poly1 < y_of poly2 && slope poly1 == SteepPositive -> RRight
>           | y_of poly1 < y_of poly2 && slope poly1 == SteepNegative -> RLeft
>           | y_of poly1 < y_of poly2 -> RBelow
>       coords -> Overlap coords
>   where y_of p = evalPoly (poly LE p) (average xrange)
>         slope [_,a] = line_slope_by_a xrange yrange a
>         slope _ = Horizontal
