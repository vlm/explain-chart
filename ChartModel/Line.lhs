> {-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

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
> import Control.Monad
> import Test.QuickCheck hiding (Positive)

> import ChartModel.SpecialPoint
> import ChartModel.Parser
> import ChartModel.Polynome
> import ChartModel.Geometry

We first say that we have 6 kinds of slopes, plus two horizontal (y = const)
and vertical line kinds. This is about right to informally describe
the usual assortment of lines used in the economic tutorials.

> data LineSlope = SlightPositive | Positive | SteepPositive
>                | SteepNegative | Negative | SlightNegative
>                | Horizontal
>                deriving (Eq, Ord, Show, Data, Typeable)

A line is either an informal line described by its slope, or somewhat
more formal line, described not only by its slope, but also by one
or more of it special points — coordinates that we know lie on the line.

> data Line = InformalLine LineSlope
>           | ExactLine {
>                       coeff_a :: Double,
>                       coeff_b :: Double
>                       }
>           deriving (Show, Data, Typeable)

An exact line is certainly an instance of polynome. Reflect it here.

> instance Polynomial Line where
>   coefficients (ExactLine a b) _ _ = [CoeffExact b, CoeffExact a]
>   coefficients (InformalLine k) xrange yrange = [CoeffAny,
>       CoeffRange Linear (adjust_aspect_ratio xrange yrange $ a_coeff_range_by k)]
>   coeff_initial_guess a xrange yrange =
>       map (guess_coeff xrange yrange) (coefficients a xrange yrange)

> guess_coeff xrange (top_right_quadrant -> range) CoeffAny = average range
> guess_coeff xrange yrange (CoeffRange Linear range) = average range
> guess_coeff xrange yrange (CoeffExact c) = c

If we knew where the line intersects with another shape, we might be able to
upgrade a line from InformalLine to ExactLine.
Otherwise, we keep line as informal as possible.

Parse the line specification.
    "A = line with positive slope"
    "B = line with negative slope"
    "C = line with slightly positive slope"
    "D = horizontal line"

> parseLine = do
>     lineSlope <- choice [
>       do { reserved "line"; reserved "with";
>            kind <- parseLineSlope; reserved "slope"; return kind },
>       reserved "horizontal" >> reserved "line" >> return Horizontal
>       ]
>     return (InformalLine lineSlope)

> parseLineSlope = choice [
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

> a_coeff_range_by :: LineSlope -> (Double, Double)
> a_coeff_range_by kind =
>  let (min_angle, max_angle) = case kind of
>       SlightPositive -> (10, 35)
>       Positive       -> (35, 60) 
>       SteepPositive  -> (60, 80)
>       SteepNegative  -> (-60, -80)
>       Negative       -> (-35, -60) 
>       SlightNegative -> (-10, -35)
>       Horizontal     -> (0.0, 0.0)
>  in (a_of_angle min_angle, a_of_angle max_angle)
>  where
>       a_of_angle angle = tan (angle * pi / 180)

Translate the polynomial coefficient back to the line slope:

> line_slope_by_a (top_right_quadrant -> (xl,xr)) (top_right_quadrant -> (yl,yr)) a =
>   let axes_ratio = (xr-xl) / (yr-yl) in
>   case 180 * (atan (a * axes_ratio)) / pi of
>     d | d >= 1 && d <= 1 -> Horizontal
>       | d > 0 && d <= 35 -> SlightPositive
>       | d > 0 && d <= 60 -> Positive
>       | d > 0 -> SteepPositive
>       | d <= -60 -> SteepNegative
>       | d <= -35 -> Negative
>       | d < 0 -> SlightNegative


> explain_line_slope n slope = case slope of
>   SteepPositive   -> line ++ " with steep positive slope"
>   SlightPositive  -> line ++ " with slight positive slope"
>   SteepNegative   -> line ++ " with steep negative slope"
>   SlightNegative  -> line ++ " with slight negative slope"
>   Negative        -> line ++ " with negative slope"
>   Positive        -> line ++ " with positive slope"
>   Horizontal      -> "horizontal " ++ line
>   where line | n == 1    = "line"
>              | otherwise = "lines"

Make random kinds of lines, for QuickCheck.

> instance Arbitrary Line where
>   arbitrary = oneof [liftM2 ExactLine arbitrary arbitrary]

