> {-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

> module ChartModel.Expression (Expression(..),
>                               showExpression,
>                               polyFromExpression,
>                               parseExpression,
>                               prop_polyDegreeMatches,
>                              ) where
> 
> import Data.Maybe
> import Data.Data hiding (Prefix, Infix)
> import Data.DeriveTH
> import Data.Generics
> import Control.Monad
> import Test.QuickCheck
> 
> import Math.Polynomial
> import qualified Text.Parsec.Expr as E
> 
> import ChartModel.Parser
> import ChartModel.Geometry
> import ChartModel.Polynome

An expression is some kind of combination of identifiers, bound
by math operations, like +, -, braces and stuff.

> data Expression =
>             EConst Double
>           | EVar String
>           | ENeg Expression
>           | ESum Expression Expression
>           | ESub Expression Expression
>           | EMul Expression Expression
>           | EDiv Expression Expression
>           | EPow Expression Expression
>           deriving (Show, Data, Typeable)

Derive Arbitrary instance for Expression.
This is needed for testing Expression with QuickCheck.

> instance Arbitrary Expression where
>   arbitrary = oneof [elements [EVar "Test"],
>                      liftM EConst arbitrary,
>                      liftM ENeg arbitrary]

Show expression in a human friendly form. This honors operator precedence,
by emitting braces where needed.

> showExpression = showExpressionPrec 0
> showExpressionPrec prec expr = case expr of
>   EConst d -> shortDouble d
>   EVar str -> str
>   ENeg a -> mbrace 0 $ "-" ++ s 0 a
>   ESum a b -> mbrace 1 $ s 1 a ++ " + " ++ s 1 b
>   ESub a b -> mbrace 1 $ s 1 a ++ " - " ++ s 1 b
>   EMul a b -> mbrace 2 $ s 2 a ++ " * " ++ s 2 b
>   EDiv a b -> mbrace 2 $ s 2 a ++ " / " ++ s 2 b
>   EPow a b -> mbrace 3 $ s 3 a ++ "^" ++ s 3 b
>  where
>   s = showExpressionPrec
>   mbrace p str = if p < prec then "(" ++ str ++ ")" else str

We build a simple expression language that allows to define shapes
in terms of other shapes. Like this:

    Line = Parabola - Line
    NegLine = -Line
    Parabola = Line^2

See http://www.haskell.org/haskellwiki/Parsing_expressions_and_statements

> parseExpression = E.buildExpressionParser table term <?> "expression"
>   where
>       table = [
>           [binary "^" EPow],
>           [E.Prefix (reservedOp "-" >> return ENeg)],
>           [binary "*" EMul, binary "/" EDiv],
>           [binary "+" ESum, binary "-" ESub]
>         ]
>       binary op f = E.Infix (reservedOp op >> return f) E.AssocLeft
>       term = parens parseExpression
>              <|> fmap EConst signedNaturalOrFloat
>              <|> fmap EVar identifier
>              <?> "simple expression"


An expression can also pose as a polynomial, but it needs to assemble
its coefficient ranges from all the dependencies. It is a non-trivial
procedure, therefore we don't automatically admit Expression into a
Polynomial type class. Instead, we make a conversion, which utilizes
a list of existing named polynomials, and the other bits of information
necessary to extract the coefficients from it, such as axes ranges.

> data PolyExpression = PExpr {
>       pe_coefficients         :: [Coefficient Double],
>       pe_coeff_initial_guess  :: [Double]
>   } deriving (Show, Data, Typeable)

> instance Polynomial PolyExpression where
>   coefficients a xrange yrange = pe_coefficients a
>   coeff_initial_guess a xrange yrange = pe_coeff_initial_guess a


> polyFromExpression :: Polynomial a => [(String, a)] -> XRange -> YRange -> Expression -> PolyExpression
> polyFromExpression shapes xrange yrange expr = case expr of
>       EConst d -> PExpr [CoeffExact d] [d]
>       ENeg (rec -> a) ->
>           PExpr (map (fmap_coeff ((-1)*)) (coeffs a))
>                 (fmap_poly negatePoly (guess a))
>       ESum (rec -> a) (rec -> b) ->
>           PExpr (zipWith join_coeff_sum (coeffs a) (coeffs b))
>                 (join_polys addPoly (guess a, guess b))
>       ESub a b -> rec (ESum a (ENeg b))
>  where coeffs a = coefficients a xrange yrange
>        guess a = coeff_initial_guess a xrange yrange
>        fmap_poly f = polyCoeffs LE . f . poly LE
>        join_polys f (p1, p2) = polyCoeffs LE $ f (poly LE p1) (poly LE p2)
>        fmap_coeff f (CoeffAny) = CoeffAny
>        fmap_coeff f (CoeffExact d) = CoeffExact (f d)
>        fmap_coeff f (CoeffRange v (l, r)) = CoeffRange v (f l, f r)
>        norm = normalizeCoefficient
>        join_coeff_sum (CoeffAny) _ = CoeffAny
>        join_coeff_sum _ (CoeffAny) = CoeffAny
>        join_coeff_sum (norm -> (CoeffRange v1 (la, ra))) (norm -> (CoeffRange v2 (lb, rb))) =
>           CoeffRange v1 (la + lb, ra + rb)
>        join_coeff_sum (norm -> (CoeffRange v1 (l, r))) (CoeffExact b) =
>           CoeffRange v1 (l + b, r + b)
>        join_coeff_sum a@(CoeffExact _) b@(CoeffRange _ _) = join_coeff_sum b a
>        join_coeff_sum (CoeffExact a) (CoeffExact b) = CoeffExact (a + b)
>        rec = polyFromExpression shapes xrange yrange


Here's a section where we define QuickCheck properties for automated tests.

The first property is to make sure that no matter what expression is,
the degree of a polynome represented by that expression must be equal to
or greater than the largest degree of a given polynomial.

How to check this property:

quickCheck prop_foo won't work, because here we can't import Line or
Parabola to avoid circular dependencies. So you'll have to fully specify
the prop type.

$ ghci src/explain-chart.lhs
*Main> quickCheck (prop_polyDegreeMatches :: [(String, Line)] -> XRange -> YRange -> Expression -> Property)

> prop_polyDegreeMatches :: (Polynomial a, Arbitrary a) => [(String, a)] -> XRange -> YRange -> Expression -> Property
> prop_polyDegreeMatches shapes xrange yrange expr =
>   let p = polyFromExpression shapes xrange yrange expr in
>   not (null shapes) && isJust (something cast expr :: Maybe String) ==>
>       length (coeffs (snd $ head shapes)) == length (coeffs p)
>   where coeffs a = coefficients a xrange yrange

