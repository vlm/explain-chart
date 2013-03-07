> {-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}

> module ChartModel.Polynome (
>                          PolyWrap(..),
>                          Polynomial(..),
>                          Coefficient(..),
>                          Variability(..),
>                          showPolynome
>                          ) where

> import Data.Data
> import Data.List

> import ChartModel.Parser
> import ChartModel.SpecialPoint
> import ChartModel.Geometry

> data Coefficient a = CoeffExact a | CoeffRange Variability (a, a) | CoeffAny
>                      deriving (Eq, Show)
> data Variability = Linear | NonLinear deriving (Eq, Show)

> data PolyWrap = forall a. (Polynomial a, Show a, Data a, Typeable a) => PolyWrap a deriving Typeable
> instance Show PolyWrap where
>   show (PolyWrap a) =
>       "PolyWrap { " ++ show a ++ " }"
> instance Data PolyWrap where
>   gfoldl k z (PolyWrap a) = z PolyWrap `k` a
>   gunfold k z c = undefined -- case constrIndex c of 1 -> (k (z PolyWrap))
>   toConstr (PolyWrap _) = con_PolyWrap
>   dataTypeOf _ = ty_T
> con_PolyWrap = mkConstr ty_T "PolyWrap" [] Prefix
> ty_T   = mkDataType "ChartModel.PolyWrap" [con_PolyWrap]

> instance Polynomial PolyWrap where
>   center_x     (PolyWrap a) = center_x a
>   center_y     (PolyWrap a) = center_y a
>   coefficients (PolyWrap a) = coefficients a
>   coeff_initial_guess (PolyWrap a) = coeff_initial_guess a
>   search_box (PolyWrap a) = search_box a

> class Polynomial a where
>    center_x :: a -> (Double, Double) -> Double
>    center_x _ = center_in_top_right_quadrant
>    center_y :: a -> (Double, Double) -> Double
>    center_y _ = center_in_top_right_quadrant
>    coefficients :: a -> XRange -> YRange -> [Coefficient Double] -- Little-Endian, e.g. a+bx+cx^2
>    coeff_initial_guess :: a -> XRange -> YRange -> [Double]
>    search_box :: a -> XRange -> YRange -> [Double]
>    search_box a xrange yrange =
>       map (default_search_box xrange yrange) (coefficients a xrange yrange)

> default_search_box xrange yrange cf = case cf of
>       CoeffAny -> 2 * abs_max yrange
>       CoeffRange _ crange -> 2 * abs_max crange
>       CoeffExact c -> 2 * c
>   where abs_max (l, r) = max (abs l) (abs r)

Show polynome as a formula (ax^2 + bx + c) with acceptable loss of precision
to shorten the output.

> showPolynome :: [Double] -> String
> showPolynome = foldr format "" . reverse . zip [0..]
>  where
>   show_x 0 d = shortDouble d
>   show_x 1 d = shortDouble d ++ "x"
>   show_x n d = shortDouble d ++ "x^" ++ show n
>   format (n, d) []         = show_x n d
>   format (n, 0) rest       = rest
>   format (n, d) ('-':rest) = show_x n d ++ " - " ++ rest
>   format (n, d) rest       = show_x n d ++ " + " ++ rest
>       

