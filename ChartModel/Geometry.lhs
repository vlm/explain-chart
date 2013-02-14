> {-# LANGUAGE ViewPatterns #-}
> module ChartModel.Geometry where

Discover the positive part of the range.

> top_right_quadrant (l, r) | l > 0     = (l, r)
>                           | otherwise = (0, r)

When x-range or y-range is specified, find the center that lies
within these coordinates, yet in the positive range.

> center_in_top_right_quadrant (top_right_quadrant -> (l, r)) =
>    l + ((r - l) / 2)
