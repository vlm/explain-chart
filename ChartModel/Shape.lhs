> {-# LANGUAGE DeriveDataTypeable #-}

> module ChartModel.Shape (Shape(..),
>                          ShapeForm(..),
>                          Shapeoid(..),
>                          mockPolyShape,
>                          isPolyForm,
>                          fromPolyForm,
>                          fromDerivedForm,
>                          parseAnyShape,
>                          topSortShapes,
>                          module ChartModel.Polynome,
>                          module ChartModel.SpecialPoint
>                          ) where

> import Data.Data
> import Data.List
> import Data.Graph
> import Test.QuickCheck
> import Control.Monad

> import ChartModel.Parser
> import ChartModel.Polynome
> import ChartModel.Line
> import ChartModel.Parabola
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

Mock a non-specific polynomial shape by giving it precise coefficient ranges
and guess coefficient values.

> mockPolyShape :: String -> [Coefficient Double] -> [Double] -> Shape
> mockPolyShape name cs guess =
>   Shape name (PolyForm $ PolyWrap (MP cs guess)) []

But what is this MP above? This is our private MockPolynome structure
that can pretend to be a Polynomial class by just returning the values it
has been initialized with during construction.

> data MockPolynome = MP {
>       mp_coeffs :: [Coefficient Double],
>       mp_guess :: [Double]
>   } deriving (Data, Typeable, Show)
> instance Polynomial MockPolynome where
>   coefficients        a _ _ = mp_coeffs a
>   coeff_initial_guess a _ _ = mp_guess a

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


> instance Arbitrary Shape where
>   arbitrary = oneof [
>       liftM3 Shape (elements ["A", "B", "C"])
>                    (fmap DerivedForm arbitrary)
>                    (return []),
>       liftM3 Shape (elements ["Poly1", "Poly2"])
>                    (fmap (PolyForm . PolyWrap) (arbitrary :: Gen Line))
>                    (return []),
>       liftM3 Shape (elements ["Poly1", "Poly2"])
>                    (fmap (PolyForm . PolyWrap) (arbitrary :: Gen Parabola))
>                    (return [])
>       ]


Topologically sort shapes so the shapes found earlier in the list
depend only on the following shapes.

> topSortShapes :: [Shape] -> [Shape]
> topSortShapes shapes =
>   let
>       -- Figuring out strongly connected components allow detecting
>       -- circular dependencies.
>       scc = stronglyConnComp (map toGraph shapes)
>       (graph, v2triple, k2v) = graphFromEdges (map toGraph shapes)
>       shape_of_vertex = (\(s, _, _) -> s) . v2triple
>   in case flattenSCCs (filter cyclic scc) of
>       -- Shapes in the head depend only on the next shapes in the list.
>       [] -> map shape_of_vertex (topSort graph)
>       shapes -> error $ "Circular dependency between "
>                      ++ intercalate " and " (map (show . name) shapes)
>   where
>       toGraph shape@(Shape { shape = PolyForm _ }) = (shape, name shape, [])
>       toGraph shape@(Shape { shape = DerivedForm expr }) =
>                   (shape, name shape, exprDependencies expr)
>       cyclic (AcyclicSCC _) = False
>       cyclic (CyclicSCC _) = True

