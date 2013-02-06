> {-# LANGUAGE NoMonomorphismRestriction #-}

Define primitive constituents of the typical chart: axes, shapes, labels.

> module ChartModel.Primitives (Primitive(..),
>                               Shape(..),
>                               parseChart, pushDownIntersections,
>                               collect, collectMap,
>                               module ChartModel.Axis,
>                               module ChartModel.Line,
>                               module ChartModel.SpecialPoint,
>                               module ChartModel.Intersection
> ) where

> import Data.List
> import Data.Data
> import Data.Generics

> import ChartModel.Shape
> import ChartModel.Axis
> import ChartModel.Line
> import ChartModel.Constraints
> import ChartModel.Intersection
> import ChartModel.SpecialPoint
> import ChartModel.Parser


Primitive is a collection of all the labels, axes, shapes, etc.

> data Primitive = MkAxis Axis
>                | MkShape Shape
>                | MkIntersection Intersection
>                deriving (Show, Data, Typeable)
> parsePrimitive =   fmap MkAxis parseAxis
>                <|> try (fmap MkShape (parseAnyShape [parseLine]))
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
>   $ everywhere (mkT $
>       \(Shape name shape []) ->
>           let ints = filter (elem name . shape_names) intersections in
>           let s_ints = concatMap (map mkSpecialPoint . coordinates) ints in
>           Shape name shape s_ints
>     ) ps
>   where intersections = collect ps :: [Intersection]
>         mkSpecialPoint (x, y) = SpecialPoint x y False ""


Collect a lists of any objects using SYB (Scrap Your Boilerplate).
Usage: collect a :: [Type]
Example
    collect chart :: [Line]

> collect = collectMap id
> collectMap f = everything (++) (\a -> case cast a of
>                                   Just a -> [f a]
>                                   Nothing -> []
>                           )

