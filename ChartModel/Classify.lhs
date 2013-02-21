
Analyze and classify the relative location of the shapes (below, above, etc),
their intersections, etc.

> {-# LANGUAGE ViewPatterns #-}
> module ChartModel.Classify where
> import Data.List
> import Data.Complex
> import Math.Polynomial
> import Numeric.GSL.Polynomials

Find the list of intersections of the given two polynomes.

> intersections :: [Double] -> [Double] -> [(Double, Double)]
> intersections (poly LE -> poly1) (poly LE -> poly2) =
>   map (\x -> (x, evalPoly poly1 x))
>   $ map realPart $ filter ((== 0) . imagPart)
>   $ polySolve $ polyCoeffs LE $ sumPolys [poly1, negatePoly poly2]

Approximate the list of intersections within the given epsilon.

> approx :: Double -> [(Double, Double)] -> [(Double, Double)]
> approx epsilon =
>   nub . sort . map (\(x, y) -> (coarse x, coarse y))
>   where coarse x = (fromIntegral $ round (x / epsilon)) * epsilon

