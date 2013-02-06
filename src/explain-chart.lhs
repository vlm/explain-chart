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

Figure out the chart dimensions.

>   let (xmin, xmax) = getAxis X chart
>   let (ymin, ymax) = getAxis Y chart

Convert intersections as DSL entities into the corresponding shapes' lists
of their intersections.

>   let chart' = pushDownIntersections chart


>   print (xmin, xmax)

> getAxis kind chart =
>   let xs = concatMap (
>             \t -> case t of
>               MkAxis axis ->
>                   if axis_kind axis == kind
>                   then
>                       [(range_min axis, range_max axis)]
>                   else
>                       []
>               _ -> []) chart in
>   case xs of
>       (x : _) -> x
>       _ -> (0, 100)
