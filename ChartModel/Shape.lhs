> {-# LANGUAGE DeriveDataTypeable #-}

> module ChartModel.Shape (Shape(..),
>                          ShapeForm(..),
>                          Shapeoid(..),
>                          isPolyForm,
>                          fromPolyForm,
>                          fromDerivedForm,
>                          parseAnyShape,
>                          module ChartModel.Polynome
>                          ) where

> import Data.Data

> import ChartModel.Parser
> import ChartModel.Polynome
> import ChartModel.Expression
> import ChartModel.SpecialPoint
> import ChartModel.Geometry

Shape is a collection of regular drawing primitives: lines, curves, etc,
as well as somewhat more complex expression combining the primitives.

> data Shape = Shape {
>               name :: String,
>               shape :: ShapeForm,
>               shape_intersections :: [SpecialPoint]
>              } deriving (Data, Typeable)
> instance Show Shape where
>   show (Shape n (PolyForm    f) si) = n ++ " = " ++ show f
>   show (Shape n (DerivedForm f) si) = n ++ " = " ++ showExpression f
> 
> data ShapeForm = PolyForm PolyWrap | DerivedForm Expression
>                  deriving (Show, Data, Typeable)
>
> isPolyForm (PolyForm _) = True
> isPolyForm (DerivedForm _) = False
> fromPolyForm (PolyForm pw) = pw
> fromPolyForm (DerivedForm _) = error "Unexpected derived form, giving up"
> fromDerivedForm (PolyForm _) = error "Unexpected polynomial form, giving up"
> fromDerivedForm (DerivedForm sf) = sf

> class Shapeoid a where
>    center_x :: a -> (Double, Double) -> Double
>    center_x _ = center_in_top_right_quadrant
>    center_y :: a -> (Double, Double) -> Double
>    center_y _ = center_in_top_right_quadrant

> instance Shapeoid ShapeForm
> instance Shapeoid Shape

There are many kinds of shapes. We don't know how to parse neither of them.
So we pick a list of all possible ShapeForm-compatible parsers and try them
in turn, returning the complete Shape.

> parseAnyShape primitive_shapes = do
>   name <- identifier
>   reservedOp "="
>   prim <- choice primitive_shapes
>   return (Shape name prim [])
