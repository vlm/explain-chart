
Parsec lets you construct parsers by combining higher-order combinators to create larger expressions.

We use Parsec to parse our chart DSL.

See also:

    http://book.realworldhaskell.org/read/using-parsec.html

> module ChartModel.Parser (run, whiteSpace, identifier,
>                           integer, natural, naturalOrFloat, signedNaturalOrFloat,
>                           reserved, reservedOp, comma, commaSep1,
>                           parens, brackets, stringLiteral,
>                           module Text.Parsec.Prim,
>                           module Text.Parsec.Char,
>                           module Text.Parsec.String,
>                           module Text.Parsec.Combinator
> ) where

> import Text.Parsec
> import Text.Parsec.Prim
> import Text.Parsec.Char
> import Text.Parsec.Combinator
> import Text.Parsec.String (Parser, parseFromFile) -- type Parser = Parsec String ()
> import Text.Parsec.Expr
> import Text.Parsec.Language (emptyDef)
> import qualified Text.Parsec.Token as P
> import Data.Char

Our language is based on C comments syntax.

> echartLangStyle :: P.LanguageDef st
> echartLangStyle = emptyDef
>       { P.commentStart   = "/*"
>       , P.commentEnd     = "*/"
>       , P.commentLine    = "//"
>       , P.nestedComments = True
>       , P.identStart     = letter
>       , P.identLetter    = alphaNum <|> oneOf "-_'"
>       , P.reservedNames  = echart_keywords
>       , P.reservedOpNames= ["=", "..", ",", "±", "+-"]
>       , P.caseSensitive  = True
>       }

> echart_keywords = ["save", "inverted", "parabola", "line", "with",
>                    "slight", "slightly", "steep", "steeply",
>                    "negative", "positive",
>                    "slope", "vertical", "horizontal",
>                    "x-axis", "y-axis", "range",
>                    "intersect", "at", "and",
>                    "goes", "through",
>                    "unlabeled", "labeled",
>                    "check", "coefficients"]

> lexer :: P.TokenParser ()
> lexer = P.makeTokenParser echartLangStyle

> whiteSpace = P.whiteSpace lexer
> lexeme     = P.lexeme lexer
> symbol     = P.symbol lexer
> integer    = P.integer lexer
> natural    = P.natural lexer
> naturalOrFloat  = do
>       v <- P.naturalOrFloat lexer
>       return $ case v of
>                   Left n  -> fromIntegral n
>                   Right d -> d
> signedNaturalOrFloat  = try $ do
>   s <- sign
>   v <- naturalOrFloat
>   return (s v)
>   where sign = (char '-' >> return negate)
>                <|> (char '+' >> return id)
>                <|> return id


> comma      = P.comma lexer
> commaSep1  = P.commaSep1 lexer
> parens     = P.parens lexer
> brackets   = P.brackets lexer
> operator   = P.operator lexer
> semi       = P.semi lexer
> identifier = P.identifier lexer
> reserved   = P.reserved lexer
> reservedOp = P.reservedOp lexer
> stringLiteral = P.stringLiteral lexer
 
> run :: Show a => Parser a -> String -> IO ()
> run p input =
>    case (Text.Parsec.parse p "" input) of
>        Left err -> do putStr "parse error at "
>                       print err
>        Right x  -> print x

