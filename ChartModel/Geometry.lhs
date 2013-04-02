> {-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}
> module ChartModel.Geometry where
> import Data.Data

In many places we use uncertain ranges for polynomial coefficients.
The Coefficient type represents what we know about the range.

The interesting part is coefficient variability profile. It is used
when we need to compute a "middle of the range". See a description for
the log_average function for details, below.

> data Variability = Linear | NonLinear deriving (Eq, Show, Data, Typeable)
> data Coefficient a =
>         CoeffExact a
>       | CoeffRange Variability (a, a)
>       | CoeffAny
>       deriving (Eq, Show, Data, Typeable)

Normalization is making sure range has left and right values in increasing order.

> normalizeCoefficient (CoeffRange v (l, r)) | l > r = CoeffRange v (r, l)
> normalizeCoefficient c = c

Make some handy type aliases to represent axis value ranges.

> type XRange = (Double, Double)
> type YRange = (Double, Double)

Compute a simple linear average of two given values. Easy!

> average (l, r) = l + (r - l) / 2

If we do a simple math average, it won't work well for ranges like [0.1..10]
for the higher degree terms (i.e., x^2). A simple math would give us 5.05, which
is not pretty. Therefore, we need to be able to compute an "average" of
such kinds of ranges by taking in account relative magnitudes. In [0.1..10]
case, the log_average will yield ~1.0.

> log_average (l, r)
>   | l > 0 && r > 0 = exp (log l + (log r - log l) / 2)
>   | l < 0 && r < 0 = -1 * log_average (-l, -r)
>   | l == 0 && r /= 0 = log_average (r / 1000, r)
>   | l == 0 && r == 0 = 0
>   | otherwise = error ("l = " ++ show l ++ ", r = " ++ show r)

Discover the positive part of the range.

> top_right_quadrant (l, r) | l > 0     = (l, r)
>                           | otherwise = (0, r)

When x-range or y-range is specified, find the center that lies
within these coordinates, yet in the positive range.

> center_in_top_right_quadrant (top_right_quadrant -> range) = average range

Select only the intersections within a certain box

> capbox :: XRange -> YRange -> [(Double, Double)] -> [(Double, Double)]
> capbox (xl, xr) (yl, yr)
>   | xl > xr   = capbox (xr, xl) (yl, yr)
>   | yl > yr   = capbox (xl, xr) (yr, yl)
>   | otherwise = dropWhile outOfRange . reverse . dropWhile outOfRange . reverse
>  where
>   outOfRange (x, y) = x < xl || x > xr || y < yl || y > yr


Shorten the text representation of a floating point number. This may lose
precision, but it is ok for most display purposes.
For example, it'll turn 2.99998 into "3" and leave 2.998 as "2.998".

> shortDouble :: Double -> String
> shortDouble d | abs d < 0.001 = (show . (read :: String -> Float) . show) d
>               | otherwise = case round (d * 1000) of
>                               n | n `rem` 1000 == 0 -> show (n `div` 1000)
>                                 | otherwise -> show $ fromIntegral n / 1000

