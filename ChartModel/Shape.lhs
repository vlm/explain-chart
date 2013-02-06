> {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

> module ChartModel.Shape (Shape(..),
>                          PolyShape(..),
>                          Coefficient(..),
>                          parseAnyShape
>                          ) where

> import Data.List

> import ChartModel.Parser
> import ChartModel.SpecialPoint

> data Coefficient a = CoeffExact a | CoeffRange (a, a) | CoeffAny
>                      deriving Eq

Shape is a collection of regular drawing primitives: lines, curves, etc.

> class PolyShape a where
>    center_x :: a -> (Int, Int) -> Double
>    center_x _ (left_x, right_x) = (fromIntegral right_x - 0) / 2
>    center_y :: a -> (Int, Int) -> Double
>    center_y _ (left_y, right_y) = (fromIntegral right_y - 0) / 2
>    coefficients :: a -> [Coefficient Double] -- Little-Endian, e.g. b+ax+cx^2

> data Shape = forall a. (Show a, PolyShape a) => Shape {
>               name :: String,
>               shape :: a,
>               shape_intersections :: [SpecialPoint]
>              }
> instance Show (Shape) where
>   show (Shape n s i) =
>       "Shape { " ++ intercalate "," [show n, show s, show i] ++ " }"

There are many kinds of shapes. We don't know how to parse neither of them.
So we pick a list of all possible PolyShape-compatible parsers and try them
in turn, returning the complete Shape.

> parseAnyShape primitive_shapes = do
>   name <- identifier
>   reservedOp "="
>   prim <- choice primitive_shapes
>   return (Shape name prim [])

