
Explain the chart in plain english for the blind people.

> {-# LANGUAGE ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}
> module ChartModel.Explain (explain) where
> import Data.List
> import Data.Ord
> import ChartModel.Geometry
> import ChartModel.Line
> import ChartModel.Parabola

> explain :: XRange -> YRange -> [(String, [Double])] -> String
> explain xrange yrange reified_shapes = intercalate " " $ concat [
>     [case (length lines, length parabolas) of
>       (0, 0) -> "The graph is empty."
>       (1, 0) -> "The graph shows a single line."
>       (0, 1) -> "The graph shows a single parabola."
>       (1, 1) -> "The graph shows a line and a parabola."
>       (n, 1) -> "The graph shows" <> n <> "lines and a parabola."
>       (1, m) -> "The graph shows a line and " <> m <> "parabolas."
>       (n, m) -> "The graph shows" <> n <> "lines and" <> m <> "parabolas."
>     ],
>     map (uncurry explain1)
>       $ map (\rshapes -> (fst $ head rshapes, map snd rshapes))
>       $ groupBy (\a b -> fst a == fst b)
>       $ sortBy (comparing fst)
>       $ map (\(name, cfs) -> (shapeClass xrange yrange cfs, name)) reified_shapes
>   ]
>   where
>       degree = length . snd
>       lines = filter ((2 ==) . degree) reified_shapes
>       parabolas = filter ((3 ==) . degree) reified_shapes

> data ShapeClass = L LineSlope | P ParType deriving (Eq, Ord, Show)
> shapeClass xrange yrange [_, a] = L $ line_slope_by_a xrange yrange a
> shapeClass xrange yrange [_, _, c] | c >= 0    = P Proper
>                                    | otherwise = P Inverted
> showShapeClass n (L k) = explain_line_slope n k
> showShapeClass 1 (P Proper)  = "parabola"
> showShapeClass 1 (P Inverted)  = "inverted parabola"
> showShapeClass n (P Proper)  = "parabolas"
> showShapeClass n (P Inverted)  = "inverted parabolas"

Explain a single group of similar shapes.

> explain1 :: ShapeClass -> [String] -> String
> explain1 sclass [name] = show name <> "is a" <> sclass ++ "."
> explain1 sclass names =
>   (intercalate ", " $ map show names) <> "are" <> (showShapeClass (length names) sclass) ++ "."

Make a singular or a plural form for English language.

> plural :: Int -> String -> String -> String -> String
> plural 0 z s m = z
> plural 1 z s m = s
> plural n z s m = m

Class Stringable and associated concatenation operator <> to
join together some common types without type conversion.

> class Show a => Stringable a where
>   s :: a -> String
>   s = show
> instance Stringable String where s = id
> instance Stringable Int
> instance Stringable ShapeClass where s = showShapeClass 1
> infixr 6  <>
> (<>) :: (Stringable a, Stringable b) => a -> b -> String
> (s -> a) <> (s -> b@(' ':_)) = s a ++ s b
> (s -> a) <> (s -> b) | null a = b
>                      | head (reverse a) == ' ' = a ++ b
>                      | otherwise = a ++ " " ++ b

