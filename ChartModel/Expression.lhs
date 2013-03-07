> {-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

> module ChartModel.Expression (Expression(..), parseExpression) where

> import Data.Data hiding (Prefix, Infix)
> import ChartModel.Parser
> import Text.Parsec.Expr

An expression is some kind of combination of identifiers, bound
by math operations, like +, -, braces and stuff.

> data Expression =
>             EConst Double
>           | EVar String
>           | EAdd Expression Expression
>           | ESub Expression Expression
>           | EMul Expression Expression
>           | EDiv Expression Expression
>           | EPow Expression Expression
>           deriving (Show, Data, Typeable)


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
>           [Prefix (reservedOp "-" >> return (ESub (EConst 0)))],
>           [binary "*" EMul, binary "/" EDiv],
>           [binary "+" EAdd, binary "-" ESub]
>         ]
>       binary op f = Infix (reservedOp op >> return f) AssocLeft
>       term = parens parseExpression
>              <|> fmap EConst signedNaturalOrFloat
>              <|> fmap EVar identifier
>              <?> "simple expression"
