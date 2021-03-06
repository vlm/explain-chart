> {-# LANGUAGE DeriveDataTypeable #-}

We should be able to parse

"x-axis range -10..10 labeled"
"y-axis range 0..10 unlabeled"

as representing the following axis layout:

                 y
                 ^
                 |       /
                 |     /
                 |   /
                 | /
      <----------+----------> x
      -10        0        10

> module ChartModel.Axis (Axis(..), AxisKind(..), defaultAxis, parseAxis) where

> import ChartModel.Parser
> import Data.Data

Axis can be y-axis or x-axis. Both can have ranges, and either of them
can be labeled or unlabeled (if the range is not important).

> data AxisKind = X | Y deriving (Show, Eq, Data, Typeable)
> data Axis = Axis {
>       axis_kind :: AxisKind,
>       range_min :: Double,
>       range_max :: Double,
>       labeled   :: Bool,
>       title     :: Maybe String
>       } deriving (Show, Data, Typeable)

> defaultAxis kind = Axis kind 0 100 False Nothing

> parseAxisKind =   (reserved "x-axis" >> return X)
>               <|> (reserved "y-axis" >> return Y)

> parseAxis = do
>     axisKind <- parseAxisKind
>     reserved "range"
>     (left, right) <- parseRange
>     isLabeled <-   (reserved "labeled" >> return True)
>                <|> (reserved "unlabeled" >> return False)
>     title <- fmap Just stringLiteral <|> return Nothing
>     return (Axis axisKind left right isLabeled title)

Parse range is ambiguous. 0..1 may confuse the floating point parser due to
the dots which look like a beginning of the floating point.

> parseRange = do
>   left <- choice [try signedNaturalOrFloat,
>                   try $ fmap fromIntegral integer]
>   reservedOp ".."
>   right <- signedNaturalOrFloat
>   case compare left right of
>       LT -> return (left, right)
>       EQ -> unexpected "Expected non-zero axis range"
>       GT -> return (right, left)

