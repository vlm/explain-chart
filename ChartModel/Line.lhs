

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

> module ChartModel.Line (Line(..), parseLine) where

> import ChartModel.SpecialPoint
> import ChartModel.Parser

We first say that we have 6 kinds of slopes, plus two horizontal (y = const)
and vertical line kinds. This is about right to informally describe
the usual assortment of lines used in the economic tutorials.

> data LineKind = SteepPositive | Positive | SlightPositive
>               | SlightNegative | Negative | SteepNegative
>               | Vertical | Horizontal
>               deriving Show

A line is either an informal line described by its slope, or somewhat
more formal line, described not only by its slope, but also by one
or more of it special points — coordinates that we know lie on the line.

> data Line = InformalLine LineKind
>           | ExactLine {
>                       coeff_a :: Int,
>                       coeff_b :: Int
>                       }
>           deriving Show

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
>       reserved "vertical"   >> reserved "line" >> return Vertical,
>       reserved "horizontal" >> reserved "line" >> return Horizontal
>       ]
>     return (InformalLine lineKind)

> parseLineKind = choice [
>     reserved "positive" >> return Positive,
>     reserved "negative" >> return Negative,
>     reserved "slightly" >> choice [
>              reserved "positive" >> return SlightPositive,
>              reserved "negative" >> return SlightNegative ],
>     reserved "steeply" >> choice [
>              reserved "positive" >> return SteepPositive,
>              reserved "negative" >> return SteepNegative ],
>     reserved "vertical" >> return Vertical,
>     reserved "horizontal">> return Horizontal
>    ]

