> {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

> module ChartModel.Shape (Shape(..),
>                          PolyShape(..),
>                          parseAnyShape,
>                          module Math.Polynomial) where

> import Data.List
> import Math.Polynomial

> import ChartModel.Parser
> import ChartModel.SpecialPoint

Shape is a collection of regular drawing primitives: lines, curves, etc.

> class PolyShape a where
>    polynome  :: a -> Poly Double

> data Shape = forall a. (Show a, PolyShape a) => Shape {
>               name :: String,
>               shape :: a,
>               shape_intersections :: [SpecialPoint]
>              }
> instance Show (Shape) where
>   show (Shape n s i) =
>       "Shape { " ++ intercalate "," [show n, show s, show i] ++ " }"

There are many kinds of shapes. We don't know how to parse them all. So we pick
a list of all possible PolyShape-compatible parsers and try them in turn,
returning the complete Shape.

> parseAnyShape primitive_shapes = do
>   name <- identifier
>   reservedOp "="
>   prim <- choice primitive_shapes
>   return (Shape name prim [])

