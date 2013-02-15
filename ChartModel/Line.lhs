> {-# LANGUAGE DeriveDataTypeable #-}

One of the simplest primitives is a simple straight unbounded line,
corresponding to a formula y = a x + b, where a represents slope
and b represents y (vertical) offset.

In our DSL, a line can be introduced via description like
"a positively sloped line", or "a line with slightly negative slope".

    "A = line with positive slope"

  y ^
    |       /
    |     /
    |   /
    | /
    +----------->
                x

    "B = line with negative slope"

  y ^
    | \
    |   \
    |     \
    |       \
    +----------->
                x

    "C = line with slightly positive slope"

  y ^
    |
    |          __
    |    __ ——
    | ——
    +----------->
                x

> module ChartModel.Line where

> import Data.Data
> import ChartModel.SpecialPoint
> import ChartModel.Parser
> import ChartModel.Shape
> import ChartModel.Geometry

We first say that we have 6 kinds of slopes, plus two horizontal (y = const)
and vertical line kinds. This is about right to informally describe
the usual assortment of lines used in the economic tutorials.

> data LineKind = SteepPositive | Positive | SlightPositive
>               | SlightNegative | Negative | SteepNegative
>               | Horizontal
>               deriving (Show, Data, Typeable)

A line is either an informal line described by its slope, or somewhat
more formal line, described not only by its slope, but also by one
or more of it special points — coordinates that we know lie on the line.

> data Line = InformalLine LineKind
>           | ExactLine {
>                       coeff_a :: Double,
>                       coeff_b :: Double
>                       }
>           deriving (Show, Data, Typeable)


An exact line is certainly an instance of polynome. Reflect it here.

> instance PolyShape Line where
>   coefficients (ExactLine a b) _ _ = [CoeffExact b,
>                                   CoeffExact a]
>   coefficients (InformalLine k) xrange yrange = [CoeffAny,
>        CoeffRange (adjust_aspect_ratio xrange yrange $ a_coeff_range_by k)]

If we knew where the line intersects with another shape, we might be able to
upgrade a line from InformalLine to ExactLine.
Otherwise, we keep line as informal as possible.

Parse the line specification.
    "A = line with positive slope"
    "B = line with negative slope"
    "C = line with slightly positive slope"
    "D = horizontal line"

> parseLine = do
>     lineKind <- choice [
>       do { reserved "line"; reserved "with";
>            kind <- parseLineKind; reserved "slope"; return kind },
>       reserved "horizontal" >> reserved "line" >> return Horizontal
>       ]
>     return (InformalLine lineKind)

> parseLineKind = choice [
>     reserved "positive" >> return Positive,
>     reserved "negative" >> return Negative,
>     choice [reserved "slight", reserved "slightly"] >> choice [
>              reserved "positive" >> return SlightPositive,
>              reserved "negative" >> return SlightNegative ],
>     choice [reserved "steep", reserved "steeply"] >> choice [
>              reserved "positive" >> return SteepPositive,
>              reserved "negative" >> return SteepNegative ],
>     reserved "horizontal">> return Horizontal
>    ]


Since the "a" coefficient corresponds to the perceived slope only in case
the coordinate system is square (i.e., x-range matches y-range), we need to
adjust it by looking at axes range ratios.

> adjust_aspect_ratio xrange yrange (a_min, a_max) =
>   let (xl, xr) = top_right_quadrant xrange
>       (yl, yr) = top_right_quadrant yrange
>       axes_ratio = (xr-xl) / (yr-yl)
>       a_min' = a_min / axes_ratio
>       a_max' = a_max / axes_ratio
>   in (a_min', a_max')

Each slope kind (except vertical) can be thought of as to correspond to a
certain range of the "a" values (from the formula a x + b).

> a_coeff_range_by :: LineKind -> (Double, Double)
> a_coeff_range_by kind =
>  let (min_angle, max_angle) = case kind of
>       SlightPositive -> (10, 35)
>       Positive       -> (35, 55) 
>       SteepPositive  -> (55, 80)
>       SteepNegative  -> (-55, -80)
>       Negative       -> (-35, -55) 
>       SlightNegative -> (-10, -35)
>       Horizontal     -> (0.0, 0.0)
>  in (a_of_angle min_angle, a_of_angle max_angle)
>  where
>       a_of_angle angle = tan (angle * pi / 180)


