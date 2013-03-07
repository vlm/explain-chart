> {-# LANGUAGE ViewPatterns #-}
> module ChartModel.Geometry where

> type XRange = (Double, Double)
> type YRange = (Double, Double)

Compute average of two given values. Easy.

> average (l, r) = l + (r - l) / 2

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
>   | otherwise = filter (\(x, y) -> x >= xl && x <= xr && y >= yl && y <= yr)


> shortDouble :: Double -> String
> shortDouble d | abs d < 0.001 = (show . (read :: String -> Float) . show) d
>               | otherwise = case round (d * 1000) of
>                               n | n `rem` 1000 == 0 -> show (n `div` 1000)
>                                 | otherwise -> show $ fromIntegral n / 1000
