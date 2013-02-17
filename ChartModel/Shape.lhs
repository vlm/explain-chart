> {-# LANGUAGE DeriveDataTypeable #-}

> module ChartModel.Shape (Shape(..),
>                          Coefficient(..),
>                          parseAnyShape,
>                          module ChartModel.Polynome
>                          ) where

> import Data.Data

> import ChartModel.Parser
> import ChartModel.Polynome
> import ChartModel.SpecialPoint

Shape is a collection of regular drawing primitives: lines, curves, etc.

> data Shape = Shape {
>               name :: String,
>               shape :: PolyWrap,
>               shape_intersections :: [SpecialPoint]
>              } deriving (Show, Data, Typeable)


There are many kinds of shapes. We don't know how to parse neither of them.
So we pick a list of all possible Polynome-compatible parsers and try them
in turn, returning the complete Shape.

> parseAnyShape primitive_shapes = do
>   name <- identifier
>   reservedOp "="
>   prim <- choice primitive_shapes
>   return (Shape name prim [])

