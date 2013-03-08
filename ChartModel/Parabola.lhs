> {-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

> module ChartModel.Parabola (Parabola(..), ParType(..), parseParabola) where
>
> import Data.Data
> import Test.QuickCheck
> 
> import ChartModel.SpecialPoint
> import ChartModel.Parser
> import ChartModel.Polynome
> import ChartModel.Geometry


Parabola can be of two general types: a proper parabola (ends up), and
an inverted parabola (ends down).

> data Parabola = Parabola ParType deriving (Show, Data, Typeable)
> data ParType = Proper | Inverted deriving (Show, Eq, Ord, Data, Typeable)

Parabola with its formula (a + bx + cx^2) is an obvious instance
of a polynomial.

> instance Polynomial Parabola where
>   coefficients p xrange yrange = [
>       CoeffAny,
>       CoeffRange Linear (-1 * par_sign p * fst xrange, -1 * par_sign p * snd xrange),
>       CoeffRange NonLinear (0.001 * par_sign p, 1 * par_sign p)
>    ]
>       where
>           par_sign (Parabola Inverted)  = -1.0
>           par_sign (Parabola Proper) =  1.0
>   coeff_initial_guess p xrange yrange =
>       zipWith (guess_coeff p xrange yrange) [0..] (coefficients p xrange yrange)

> guess_coeff (Parabola Inverted) xrange yrange 0 CoeffAny = 0
> guess_coeff (Parabola Proper) xrange (ybtm, ytop) 0 CoeffAny = ytop
> guess_coeff p xrange yrange _ (CoeffRange Linear range) = average range
> guess_coeff p xrange yrange _ (CoeffRange NonLinear range) = log_average range
> guess_coeff p xrange yrange _ (CoeffExact c) = c

Parabola is represented in the DSL as a simple
    X = parabola
or
    X = inverted parabola

> parseParabola = do
>     t <- (reserved "inverted" >> return Inverted) <|> return Proper
>     reserved "parabola"
>     return (Parabola t)

Make random kinds of parabolas, for QuickCheck.

> instance Arbitrary Parabola where
>   arbitrary = elements [Parabola Proper, Parabola Inverted]

