
Define primitive constituents of the typical chart: axes, shapes, labels.

> module ChartModel.Primitives (parseChart, pushDownIntersections, Shape(..),
>                               module ChartModel.Axis,
>                               module ChartModel.Line,
>                               module ChartModel.SpecialPoint,
>                               module ChartModel.Intersection
> ) where

> import Data.List

> import ChartModel.Axis
> import ChartModel.Line
> import ChartModel.Intersection
> import ChartModel.SpecialPoint
> import ChartModel.Parser


Shape is a collection of drawing primitives: lines, curves, etc.

> data Shape a = Shape {
>               name :: String,
>               shape :: a,
>               shape_intersections :: [SpecialPoint]
>              } deriving Show
> parseShape = do
>   name <- identifier
>   reservedOp "="
>   line <- parseLine
>   return (Shape name line [])


Primitive is a collection of all the labels, axes, shapes, etc.

> data Primitive = MkAxis Axis
>                | MkShape (Shape Line)
>                | MkIntersection Intersection
>                deriving Show
> parsePrimitive =   fmap MkAxis parseAxis
>                <|> try (fmap MkShape parseShape)
>                <|> try (fmap MkIntersection parseIntersection)

> isIntersection p = case p of { MkIntersection _ -> True; _ -> False }


Parsing the chart file is easy: just parse all primitives out of the file
until EOF is reached, and return a list of them.

> parseChart = do
>   whiteSpace
>   primitives <- many parsePrimitive
>   eof
>   return primitives


Now, hopefully we have some intersections defined for our shapes. We can
distribute the intersection information to the respective shapes by removing
the intersections from the primitives and adding the intersection coordinates
separately to each shape. This algorithm is quadratic, but who cares.

> pushDownIntersections ps =
>   filter (not . isIntersection)
>   $ map (\p -> case p of
>       MkShape (Shape name shape []) ->
>           let ints = filter (elem name . shape_names) intersections in
>           let s_ints = concatMap (map mkSpecialPoint . coordinates) ints in
>           MkShape (Shape name shape s_ints)
>       x -> x
>     ) ps
>   where intersections = map (\(MkIntersection x) -> x) $ filter isIntersection ps
>         mkSpecialPoint (x, y) = SpecialPoint x y False ""


