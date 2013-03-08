> {-# LANGUAGE DeriveDataTypeable, OverlappingInstances, FlexibleInstances, ViewPatterns #-}

> module ChartModel.Expression (Expression(..),
>                               NamedPolynomial(..),
>                               showExpression,
>                               listifyWholeLists,
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
> polyFromExpression polys xrange yrange expr = case expr of
>       EConst d -> PExpr [CoeffExact d] [d]
>       EVar name -> case (lookup name polys) of
>                       Nothing -> error $ name ++ " used in expression"
>                                          ++ " is not defined in chart file"
>                       Just p -> PExpr (coeffs p) (guess p)
>       ENeg (rec -> a) ->
>           PExpr (map (fmap_coeff ((-1)*)) (coeffs a))
>                 (fmap_poly negatePoly (guess a))
>       ESum (rec -> a) (rec -> b) ->
>           PExpr (zip' (replaceUndefined join_coeff_sum) (coeffs a) (coeffs b))
>                 (join_polys addPoly (guess a, guess b))
>       ESub a b -> rec (ESum a (ENeg b))
>  where coeffs a = coefficients a xrange yrange
>        guess a = coeff_initial_guess a xrange yrange
>        norm = normalizeCoefficient
>        fmap_poly f = polyCoeffs LE . f . poly LE
>        join_polys f (p1, p2) = polyCoeffs LE $ f (poly LE p1) (poly LE p2)
>        fmap_coeff f (CoeffAny) = CoeffAny
>        fmap_coeff f (CoeffExact d) = CoeffExact (f d)
>        fmap_coeff f (CoeffRange v (l, r)) = CoeffRange v (f l, f r)
>        join_coeff_sum (CoeffAny) _ = CoeffAny
>        join_coeff_sum _ (CoeffAny) = CoeffAny
>        join_coeff_sum (norm -> (CoeffRange v1 (la, ra))) (norm -> (CoeffRange v2 (lb, rb))) =
>           CoeffRange v1 (la + lb, ra + rb)
>        join_coeff_sum (norm -> (CoeffRange v1 (l, r))) (CoeffExact b) =
>           CoeffRange v1 (l + b, r + b)
>        join_coeff_sum a@(CoeffExact _) b@(CoeffRange _ _) = join_coeff_sum b a
>        join_coeff_sum (CoeffExact a) (CoeffExact b) = CoeffExact (a + b)
>        -- Domain-specific zip, which does not strip the longest list's tail
>        zip' f (a:as) (b:bs) = f (Just a) (Just b) : zip' f as bs
>        zip' f [] bs = map (f Nothing . Just) bs
>        zip' f as [] = map (flip f Nothing . Just) as
>        replaceUndefined f Nothing (Just b) = b
>        replaceUndefined f (Just a) Nothing = a
>        replaceUndefined f (Just a) (Just b) = f a b
>        rec = polyFromExpression polys xrange yrange


Here's a section where we define QuickCheck properties for automated tests.

First we derive Arbitrary instance for Expression to produce random expressions.

> instance Arbitrary Expression where
>   arbitrary = oneof [return (EVar "Poly1"),
>                      return (EVar "Poly2"),
>                      liftM EConst arbitrary,
>                      liftM ENeg arbitrary,
>                      liftM2 ESum arbitrary arbitrary,
>                      liftM2 ESub arbitrary arbitrary
>                     ]
>   shrink (EConst d) = map EConst (shrink d)
>   shrink (EVar "Poly2") = [EVar "Poly1"]
>   shrink (ENeg a) = [a]
>   shrink (ESum a b) = [a, b]
>   shrink (ESub a b) = [a, b]
>   shrink (EMul a b) = [a, b]
>   shrink (EDiv a b) = [a, b]
>   shrink (EPow a b) = [a, b]
>   shrink a = []

The first property asserts that no matter what expression is,
the degree of a polynome represented by that expression must be equal to
or greater than the largest degree of a given polynomial. For example,
if we describe in the chart something like

    P = parabola
    X = P - 3

then we know that X must have a degree of 3, since X is also a parabola.

How to check this property? A simple `quickCheck prop_polyDegreeMatches`
won't work, because here we can't import Line or Parabola to avoid circular
dependencies. So you'll have to fully specify the property type.

$ ghci src/explain-chart.lhs
*Main> quickCheck (prop_polyDegreeMatches :: [NamedPolynomial Line] -> XRange -> YRange -> Expression -> Property)
*Main> quickCheck (prop_polyDegreeMatches :: [NamedPolynomial Parabola] -> XRange -> YRange -> Expression -> Property)

> prop_polyDegreeMatches :: (Polynomial a, Arbitrary a) => [NamedPolynomial a] -> XRange -> YRange -> Expression -> Property
> prop_polyDegreeMatches polys xrange yrange expr =
>   let p = polyFromExpression (map named polys) xrange yrange expr
>       namesInExpr = listifyWholeLists expr    -- Shapes used by expression
>       polysInExpr = filter ((`elem` namesInExpr) . np_name) polys
>   in
>   not (null polysInExpr)
>   && all (`elem` (map np_name polys)) namesInExpr ==>
>       maximum (map (length . coeffs . np_poly) polysInExpr) <= length (coeffs p)
>   where
>       coeffs a = coefficients a xrange yrange
>       named s = (np_name s, np_poly s)

> data NamedPolynomial a = NP {
>       np_name :: String,
>       np_poly :: a
>   } deriving (Show, Data, Typeable)

> instance Arbitrary a => Arbitrary [NamedPolynomial a] where
>   arbitrary = listOf $ oneof [
>                       liftM (NP "Poly1") arbitrary,
>                       liftM (NP "Poly2") arbitrary
>                     ]
>   shrink [NP "Poly2" x] = [[NP "Poly1" x]]
>   shrink [x] = []
>   shrink xs = map return xs

A version of Data.Generics.listify which doesn't recurse into sublists of type [b]
Adapted http://www.haskell.org/haskellwiki/Scrap_your_boilerplate

> listifyWholeLists :: Typeable b => GenericQ [[b]]
> listifyWholeLists = flip (synthesize id (.) (mkQ id (\bl _ -> (bl:)))) []

