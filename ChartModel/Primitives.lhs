
Define primitive constituents of the typical chart: axes, shapes, labels.

> module ChartModel.Primitives (parseChart
> ) where

> import ChartModel.Axis
> import ChartModel.Line
> import ChartModel.Intersection
> import ChartModel.Parser


Shape is a collection of drawing primitives: lines, curves, etc.

> data Shape = Line Line    deriving Show
> parseShape = fmap Line parseLine


Primitive is a collection of all the labels, axes, shapes, etc.

> data Primitive = MkAxis Axis
>                | MkShape Shape
>                | MkIntersection Intersection
>                deriving Show
> parsePrimitive =   fmap MkAxis parseAxis
>                <|> try (fmap MkShape parseShape)
>                <|> try (fmap MkIntersection parseIntersection)

> parseChart = do
>   whiteSpace
>   primitives <- many parsePrimitive
>   eof
>   return primitives

