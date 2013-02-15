> {-# LANGUAGE NoMonomorphismRestriction #-}

Define primitive constituents of the typical chart: axes, shapes, labels.

> module ChartModel.Primitives (Filename, ChartStmt(..),
>                               parseChart, pushDownIntersections,
>                               collect, collectMap, check_coefficients,
>                               module ChartModel.Axis,
>                               module ChartModel.Line,
>                               module ChartModel.Constraints,
>                               module ChartModel.Shape,
>                               module ChartModel.SpecialPoint,
>                               module ChartModel.Intersection
> ) where

> import Data.List
> import Data.Maybe
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

> newtype Filename = Filename String deriving (Data, Typeable)
> instance Show Filename where
>     show (Filename name) = name

> data ChartStmt = MkAxis Axis
>                | MkShape Shape
>                | MkSave Filename
>                | MkCoeffCheck CoeffCheck
>                | MkIntersection Intersection
>                deriving (Show, Data, Typeable)
> parseChartStatement =   fmap MkAxis parseAxis
>   <|> try (fmap MkShape (parseAnyShape [parseLine]))
>   <|> try (fmap MkSave (fmap Filename $ reserved "save" >> stringLiteral))
>   <|> try (fmap MkCoeffCheck parseCoeffCheck)
>   <|> try (fmap MkIntersection parseIntersection)

> isIntersection p = case p of { MkIntersection _ -> True; _ -> False }


Parse debugging assertion which checks whether polynomial coefficients
lie in a specified range.

check polynomial coefficients for Line1 [45.5 ± 0.5, -3.5 ± 1]

> data CoeffCheck = CoeffCheck {
>           cc_shape_name :: String,
>           cc_coeffs     :: [(Double, Double)]
>       } deriving (Show, Data, Typeable)
> parseCoeffCheck = do
>   mapM_ reserved ["check", "polynomial", "coefficients", "for"]
>   name <- identifier
>   coeffs <- brackets $ commaSep1 valueAndRange
>   return (CoeffCheck name coeffs)
>   where
>       valueAndRange = choice [
>           try $ do
>               v <- signedNaturalOrFloat
>               choice (map reservedOp ["±", "+-"])
>               r <- naturalOrFloat
>               return (v, abs r)
>           , try $ do
>               v <- signedNaturalOrFloat
>               return (v, 0.01 * abs v)    -- Err within 1% by default.
>           ]

> check_coefficients :: String -> [ChartStmt] -> [Double] -> Maybe String
> check_coefficients name chart actual_coeffs =
>   let checks = filter (\cc -> cc_shape_name cc == name) (collect chart :: [CoeffCheck]) in
>   case mapMaybe (verify actual_coeffs . cc_coeffs) checks of
>       [] -> Nothing
>       ss -> Just (intercalate "\n" ss)
>   where
>   verify actual_coeffs supposed_coeffs =
>      if length actual_coeffs /= length supposed_coeffs
>      then
>           Just $ "Wrong number of coefficients for " ++ name
>                  ++  ": "  ++ show (length actual_coeffs)
>                  ++ " != " ++ show (length supposed_coeffs)
>      else
>           case catMaybes $ zipWith inrange actual_coeffs supposed_coeffs of
>               []   -> Nothing
>               errs -> Just $ "Coefficient check failed for " ++ name ++ ": "
>                               ++ intercalate ", " errs
>   inrange coeff (v, erange) =
>       if coeff > v - erange && coeff < v + erange
>       then Nothing
>       else Just (show coeff ++ " != " ++ show v ++ " ± " ++ show erange)

parsing the chart file is easy: just parse all chart components out of the file
until EOF is reached, and return a list of them.

> parseChart = do
>   whiteSpace
>   statements <- many parseChartStatement
>   eof
>   return statements


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

