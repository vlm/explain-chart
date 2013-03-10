> {-# LANGUAGE DeriveDataTypeable, OverlappingInstances, FlexibleInstances, ViewPatterns #-}

> module ChartModel.Expression (Expression(..),
>                               showExpression,
>                               evalExpr,
>                               exprDependencies,
>                               parseExpression,
>                              ) where
> 
> import Data.Maybe
> import Data.Data hiding (Prefix, Infix)
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
>           | EName String
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
>   EName str -> str
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
>              <|> fmap EName identifier
>              <?> "simple expression"

Evaluate the expression at a specified point given the function which
translates the name of the dependent shape into an evaluation function
for that shape.

> evalExpr :: (String -> Double -> Double) -> Expression -> Double -> Double
> evalExpr resolve expr x = case expr of
>   EConst d -> d
>   EName n -> resolve n x
>   ENeg e -> - (evalExpr resolve e x)
>   ESum a b -> (+) (evalExpr resolve a x) (evalExpr resolve b x)
>   ESub a b -> (-) (evalExpr resolve a x) (evalExpr resolve b x)
>   EMul a b -> (*) (evalExpr resolve a x) (evalExpr resolve b x)
>   EDiv a b -> (/) (evalExpr resolve a x) (evalExpr resolve b x)
>   EPow a b -> (**) (evalExpr resolve a x) (evalExpr resolve b x)

Here's a section where we define QuickCheck properties for automated tests.

First we derive Arbitrary instance for Expression to produce random expressions.

> instance Arbitrary Expression where
>   arbitrary = oneof [return (EName "Poly1"),
>                      return (EName "Poly2"),
>                      liftM EConst arbitrary,
>                      liftM ENeg arbitrary,
>                      liftM2 ESum arbitrary arbitrary,
>                      liftM2 ESub arbitrary arbitrary
>                     ]
>   shrink (EConst d) = map EConst (shrink d)
>   shrink (EName "Poly2") = [EName "Poly1"]
>   shrink (ENeg a) = [a]
>   shrink (ESum a b) = [a, b]
>   shrink (ESub a b) = [a, b]
>   shrink (EMul a b) = [a, b]
>   shrink (EDiv a b) = [a, b]
>   shrink (EPow a b) = [a, b]
>   shrink a = []

A version of Data.Generics.listify which doesn't recurse into sublists of type [b]
Adapted http://www.haskell.org/haskellwiki/Scrap_your_boilerplate

> listifyWholeLists :: Typeable b => GenericQ [[b]]
> listifyWholeLists = flip (synthesize id (.) (mkQ id (\bl _ -> (bl:)))) []

> exprDependencies :: Expression -> [String]
> exprDependencies = listifyWholeLists

