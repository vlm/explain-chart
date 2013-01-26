
Define primitive constituents of the typical chart: axes, shapes, labels.

> module ChartModel.Primitives (parseChart,
>                               module ChartModel.Axis,
>                               module ChartModel.Line
> ) where

> import ChartModel.Axis
> import ChartModel.Line
> import ChartModel.Parser


Shape is a collection of drawing primitives: lines, curves, etc.

> data Shape = Line Line    deriving Show
> parseShape = fmap Line parseLine


Primitive is a collection of all the labels, axes, shapes, etc.

> data Primitive = MkAxis Axis | MkShape Shape    deriving Show
> parsePrimitive =   fmap MkAxis parseAxis
>                <|> fmap MkShape parseShape

> parseChart = do
>   whiteSpace
>   many parsePrimitive

