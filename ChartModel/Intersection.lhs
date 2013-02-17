> {-# LANGUAGE DeriveDataTypeable #-}

To constrain the shapes we should be able to parse

    "C, D, E intersect at (3, 3) and (4, 5)"
    "A, B intersect at (2, 2)", like shown:

       y
       ^        A
       |       /
       |     /
     2 |___/______ B
       | /
       +----------> x
       0   2     5

> module ChartModel.Intersection (Intersection(..), parseIntersection) where

> import Data.Data
> import ChartModel.Parser

We define which shapes intersect, and a number of coordinate pairs
where intersections occur.

> data Intersection = Intersection {
>       shape_names :: [String],
>       coordinates :: [(Double, Double)]
>       } deriving (Show, Data, Typeable)

> parseIntersection = do
>     shapes <- choice [ try $ do
>                           shapes <- andSep2 identifier
>                           mapM reserved ["intersect", "at"]
>                           return shapes
>                       , try $ do
>                           shape <- identifier
>                           mapM reserved ["goes", "through"]
>                           return [shape]
>                       ]
>     ints <- (parens $ do
>                   x <- fmap fromIntegral natural
>                   comma
>                   y <- fmap fromIntegral natural
>                   return (x, y)
>              ) `sepBy1` (reserved "and")
>     return (Intersection shapes ints)


Define a helper function that can capture 2 or more "and"-separated things.

> andSep2 p = do
>   first <- p
>   reserved "and"
>   rest <- p `sepBy1` (reserved "and")
>   return (first : rest)

