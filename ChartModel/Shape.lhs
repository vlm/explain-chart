> {-# LANGUAGE DeriveDataTypeable #-}

> module ChartModel.Shape (Shape(..),
>                          ShapeForm(..),
>                          Coefficient(..),
>                          isPolyForm,
>                          fromPolyForm,
>                          fromShapeForm,
>                          parseAnyShape,
>                          module ChartModel.Polynome
>                          ) where

> import Data.Data

> import ChartModel.Parser
> import ChartModel.Polynome
> import ChartModel.Expression
> import ChartModel.SpecialPoint

Shape is a collection of regular drawing primitives: lines, curves, etc,
as well as somewhat more complex expression combining the primitives.

> data Shape = Shape {
>               name :: String,
>               shape :: ShapeForm,
>               shape_intersections :: [SpecialPoint]
>              } deriving (Data, Typeable)
> instance Show Shape where
>   show (Shape n (PolyForm f) si) = n ++ " = " ++ show f
>   show (Shape n (ExprForm f) si) = n ++ " = " ++ showExpression f
> 
> data ShapeForm = PolyForm PolyWrap | ExprForm Expression
>                  deriving (Show, Data, Typeable)
>
> isPolyForm (PolyForm _) = True
> isPolyForm (ExprForm _) = False
> fromPolyForm (PolyForm pw) = pw
> fromPolyForm (ExprForm _) = error "Unexpected ExpForm, giving up"
> fromShapeForm (PolyForm _) = error "Unexpected ShapeForm, giving up"
> fromShapeForm (ExprForm sf) = sf

There are many kinds of shapes. We don't know how to parse neither of them.
So we pick a list of all possible ShapeForm-compatible parsers and try them
in turn, returning the complete Shape.

> parseAnyShape primitive_shapes = do
>   name <- identifier
>   reservedOp "="
>   prim <- choice primitive_shapes
>   return (Shape name prim [])

