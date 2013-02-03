> import System.Console.GetOpt (getOpt, ArgOrder(..))
> import System.Environment (getArgs)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hPutStrLn, stderr)

> import ChartModel.Primitives
> import ChartModel.Parser

> main = do
>   args <- getArgs
>   chart <- case getOpt RequireOrder [] args of
>                   (_, [filename], []) -> do
>                       result <- parseFromFile parseChart filename
>                       case result of
>                           Left err  -> do
>                               hPutStrLn stderr (show err)
>                               exitWith (ExitFailure 1)
>                           Right xs  -> return xs
>                   (_, _, errs) -> do
>                       hPutStrLn stderr (concat errs ++ "Usage: explain-chart <filename>")
>                       exitWith (ExitFailure 1)
>   print chart
