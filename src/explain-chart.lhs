> import System.Console.GetOpt (getOpt, ArgOrder(..))
> import System.Environment (getArgs)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hPutStrLn, stderr)

> import ChartModel.Primitives
> import ChartModel.Parser

> data Flag = Flag

> main = do
>     args <- getArgs
>     case getOpt RequireOrder [] args of
>         (_, [filename], []) -> do
>             result <- parseFromFile parseChart filename
>             case result of
>                 Left err  -> print err
>                 Right xs  -> print xs
>         (_, _, errs) -> do
>             hPutStrLn stderr (concat errs ++ "Usage: explain-chart <filename>")
>             exitWith (ExitFailure 1)

