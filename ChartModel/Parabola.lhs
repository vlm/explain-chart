> {-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

> module ChartModel.Parabola (Parabola(..), parseParabola) where

> import Data.Data
> import ChartModel.SpecialPoint
> import ChartModel.Parser
> import ChartModel.Polynome
> import ChartModel.Geometry

> data Parabola = Par { inverted :: Bool }
>           deriving (Show, Data, Typeable)


> instance Polynomial Parabola where
>   coefficients p xrange yrange = [
>       CoeffAny,
>       CoeffRange (-1 * par_sign p * fst xrange, -1 * par_sign p * snd xrange),
>       CoeffRange (0.01 * par_sign p, par_sign p)
>    ]
>   coeff_initial_guess p xrange yrange =
>       zipWith (guess_coeff p xrange yrange) [0..] (coefficients p xrange yrange)

> guess_coeff p xrange yrange _ (CoeffExact c) = c
> guess_coeff (Par True) xrange yrange 0 (CoeffRange range) = 0
> guess_coeff (Par False) xrange yrange 0 (CoeffRange range) = average range
> guess_coeff p xrange yrange 1 (CoeffRange range) = log_average 2 range
> guess_coeff p xrange yrange 2 (CoeffRange range) = log_average 2 range
> guess_coeff (Par True) xrange yrange 0 CoeffAny = 0
> guess_coeff (Par False) xrange (ybtm, ytop) 0 CoeffAny = ytop

> parseParabola = do
>     inv <- (reserved "inverted" >> return True) <|> return False
>     reserved "parabola"
>     return (Par inv)

> par_sign (Par True)  = -1.0
> par_sign (Par False) =  1.0
