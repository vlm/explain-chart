> {-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, ViewPatterns #-}

> module ChartModel.Shape (Shape(..),
>                          PolyShape(..),
>                          Coefficient(..),
>                          parseAnyShape
>                          ) where

> import Data.Data
> import Data.List

> import ChartModel.Parser
> import ChartModel.SpecialPoint
> import ChartModel.Geometry

> data Coefficient a = CoeffExact a | CoeffRange (a, a) | CoeffAny
>                      deriving (Eq, Show)

Shape is a collection of regular drawing primitives: lines, curves, etc.

> class PolyShape a where
>    center_x :: a -> (Double, Double) -> Double
>    center_x _ = center_in_top_right_quadrant
>    center_y :: a -> (Double, Double) -> Double
>    center_y _ = center_in_top_right_quadrant
>    coefficients :: a -> (Double, Double) -> (Double, Double) -> [Coefficient Double] -- Little-Endian, e.g. b+ax+cx^2
>    coeff_initial_guess :: a -> (Double, Double) -> (Double, Double) -> [Double]
>    coeff_initial_guess a xrange yrange =
>       map (guess_coefficient xrange yrange) (coefficients a xrange yrange)

> guess_coefficient xrange yrange (CoeffExact c) = c
> guess_coefficient xrange yrange (CoeffRange (l, r)) = l + (r-l)/2
> guess_coefficient xrange (top_right_quadrant -> (yl,yr)) (CoeffAny) = yr

> data Shape = forall a. (Show a, PolyShape a, Data a, Typeable a) => Shape {
>               name :: String,
>               shape :: a,
>               shape_intersections :: [SpecialPoint]
>              } deriving (Typeable)
> instance Show Shape where
>   show (Shape n s i) =
>       "Shape { " ++ intercalate "," [show n, show s, show i] ++ " }"
> instance Data Shape where
>   gfoldl k z (Shape n s i) = z Shape `k` n `k` s `k` i
>   gunfold k z c = undefined -- case constrIndex c of 1 -> k (k (k (z Shape)))
>   toConstr (Shape _ _ _) = con_Shape
>   dataTypeOf _ = ty_T
> con_Shape = mkConstr ty_T "Shape" [] Prefix
> ty_T   = mkDataType "ChartModel.Shape" [con_Shape]


There are many kinds of shapes. We don't know how to parse neither of them.
So we pick a list of all possible PolyShape-compatible parsers and try them
in turn, returning the complete Shape.

> parseAnyShape primitive_shapes = do
>   name <- identifier
>   reservedOp "="
>   prim <- choice primitive_shapes
>   return (Shape name prim [])

