> {-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

> module ChartModel.Expression (Expression(..),
>                               showExpression,
>                               parseExpression) where

> import Data.Data hiding (Prefix, Infix)
> import Text.Parsec.Expr
> import ChartModel.Parser
> import ChartModel.Geometry

An expression is some kind of combination of identifiers, bound
by math operations, like +, -, braces and stuff.

> data Expression =
>             EConst Double
>           | EVar String
>           | ENeg Expression
>           | EAdd Expression Expression
>           | ESub Expression Expression
>           | EMul Expression Expression
>           | EDiv Expression Expression
>           | EPow Expression Expression
>           deriving (Show, Data, Typeable)

> showExpression = showExpressionPrec 0
> showExpressionPrec prec expr = case expr of
>   EConst d -> shortDouble d
>   EVar str -> str
>   ENeg a -> mbrace 0 $ "-" ++ s 0 a
>   EAdd a b -> mbrace 1 $ s 1 a ++ " + " ++ s 1 b
>   ESub a b -> mbrace 1 $ s 1 a ++ " - " ++ s 1 b
>   EMul a b -> mbrace 2 $ s 2 a ++ " * " ++ s 2 b
>   EDiv a b -> mbrace 2 $ s 2 a ++ " / " ++ s 2 b
>   EPow a b -> mbrace 3 $ s 3 a ++ "^" ++ s 3 b
>  where
>   s = showExpressionPrec
>   mbrace p str = if p < prec then "(" ++ str ++ ")" else str

We build a simple language that allows to define shapes in terms of
other shapes. Like this:

    Line = Parabola - Line
    NegLine = -Line
    Parabola = Line^2

See http://www.haskell.org/haskellwiki/Parsing_expressions_and_statements

> parseExpression = buildExpressionParser table term <?> "expression"
>   where
>       table = [
>           [binary "^" EPow],
>           [Prefix (reservedOp "-" >> return ENeg)],
>           [binary "*" EMul, binary "/" EDiv],
>           [binary "+" EAdd, binary "-" ESub]
>         ]
>       binary op f = Infix (reservedOp op >> return f) AssocLeft
>       term = parens parseExpression
>              <|> fmap EConst signedNaturalOrFloat
>              <|> fmap EVar identifier
>              <?> "simple expression"
