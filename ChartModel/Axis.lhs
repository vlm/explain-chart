
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

> module ChartModel.Axis (Axis(..), AxisKind(..), parseAxis) where

> import ChartModel.Parser
> import Data.Data

Axis can be y-axis or x-axis. Both can have ranges, and either of them
can be labeled or unlabeled (if the range is not important).

> data AxisKind = X | Y deriving (Show, Eq, Data, Typeable)
> data Axis = Axis {
>       axis_kind :: AxisKind,
>       range_min :: Int,
>       range_max :: Int,
>       labeled   :: Bool
>       } deriving (Show, Data, Typeable)

> parseAxisKind =   (reserved "x-axis" >> return X)
>               <|> (reserved "y-axis" >> return Y)

> parseAxis = do
>     axisKind <- parseAxisKind
>     reserved "range"
>     left <- fmap fromIntegral natural
>     reservedOp ".."
>     right <- fmap fromIntegral natural
>     isLabeled <-   (reserved "labeled" >> return True)
>                <|> (reserved "unlabeled" >> return False)
>     return (Axis axisKind left right isLabeled)

