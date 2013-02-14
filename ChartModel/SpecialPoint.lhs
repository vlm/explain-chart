> {-# LANGUAGE DeriveDataTypeable #-}

Special points are coordinates that lie on a particular shape.

> module ChartModel.SpecialPoint (SpecialPoint(..), sp_xy) where

> import Data.Data

A SpecialPoint might be represented by a pair of coordinates (x,y).
However, we also need to know whether these coordinates are to be specifically
highlighed on the graph, or just happen to be one of many possible points
lying on the shape bearing no special meaning.

> data SpecialPoint = SpecialPoint {
>                         sp_x :: Double,
>                         sp_y :: Double,
>                         special :: Bool,
>                         label  :: String
>                     }
>                    deriving (Show, Data, Typeable)

> sp_xy sp = (sp_x sp, sp_y sp)
