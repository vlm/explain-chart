> {-# LANGUAGE ViewPatterns #-}
> module ChartModel.Geometry where

Compute average of two given values. Easy.

> average (l, r) = l + (r - l) / 2

> log_average b (l, r)
>   | l > 0 && r > 0 = b ** (logBase b l + (logBase b r - logBase b l) / 2)
>   | l < 0 && r < 0 = -1 * log_average b (-l, -r)
>   | l == 0 && r /= 0 = log_average b (r / 1000, r)
>   | l == 0 && r == 0 = 0
>   | otherwise = error ("l = " ++ show l ++ ", r = " ++ show r)

Discover the positive part of the range.

> top_right_quadrant (l, r) | l > 0     = (l, r)
>                           | otherwise = (0, r)

When x-range or y-range is specified, find the center that lies
within these coordinates, yet in the positive range.

> center_in_top_right_quadrant (top_right_quadrant -> range) = average range
