> {-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

> module ChartModel.Parabola (Parabola, parseParabola) where

> import Data.Data
> import ChartModel.SpecialPoint
> import ChartModel.Parser
> import ChartModel.Polynome
> import ChartModel.Geometry

> data Parabola = Par ParType deriving (Show, Data, Typeable)
> data ParType = Proper | Inverted deriving (Show, Data, Typeable)


> instance Polynomial Parabola where
>   coefficients p xrange yrange = [
>       CoeffAny,
>       CoeffRange (-1 * par_sign p * fst xrange, -1 * par_sign p * snd xrange),
>       CoeffRange (0.01 * par_sign p, par_sign p)
>    ]
>   coeff_initial_guess p xrange yrange =
>       zipWith (guess_coeff p xrange yrange) [0..] (coefficients p xrange yrange)

> guess_coeff (Par Inverted) xrange yrange 0 CoeffAny = 0
> guess_coeff (Par Proper) xrange (ybtm, ytop) 0 CoeffAny = ytop
> guess_coeff (Par Inverted) xrange yrange 0 (CoeffRange range) = 0
> guess_coeff (Par Proper) xrange yrange 0 (CoeffRange range) = average range
> guess_coeff p xrange yrange _ (CoeffRange range) = log_average 2 range
> guess_coeff p xrange yrange _ (CoeffExact c) = c

> parseParabola = do
>     t <- (reserved "inverted" >> return Inverted) <|> return Proper
>     reserved "parabola"
>     return (Par t)

> par_sign (Par Inverted)  = -1.0
> par_sign (Par Proper) =  1.0
