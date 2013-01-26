
Special points are coordinates that lie on a particular shape.

> module ChartModel.SpecialPoint (SpecialPoint(..)) where

A SpecialPoint might be represented by a pair of coordinates (x,y).
However, we also need to know whether these coordinates are to be specifically
highlighed on the graph, or just happen to be one of many possible points
lying on the shape bearing no special meaning.

> data SpecialPoint = SpecialPoint {
>                         sp_x :: Int,
>                         sp_y :: Int,
>                         special :: Bool,
>                         label  :: String
>                     }
>                    deriving Show

