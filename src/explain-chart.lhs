> {-# LANGUAGE BangPatterns #-}
> import System.Console.GetOpt
> import System.Environment (getArgs)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hPutStr, hPutStrLn, stderr)

> import Data.Data
> import Data.List
> import Data.Maybe
> import Data.Generics
> import Control.Monad (when)

The chart is described by some DSL. It is useful to visually
check what that DSL describes. We employ Chart library to draw the graphs.

> import Graphics.Rendering.Chart.Simple
> import Graphics.Rendering.Chart.Renderable
> import Graphics.Rendering.Chart
> import Data.Accessor
> import Data.Colour
> import Data.Colour.SRGB.Linear

A different way to plot, useful for 3D debugging.

> import Graphics.Plot
> import Data.Packed.Matrix

We use numeric minimization to compute the optimal shape of the graph,
given various constraints. Constraints may include a list of proposed
intersections, a desired center of the graph, and so on.

> import Numeric.GSL.Minimization
> import Math.Polynomial

> import ChartModel.Primitives
> import ChartModel.Parser

> import Debug.Trace

> data Flag = Debug deriving Eq
> cliOptions :: [OptDescr Flag]
> cliOptions = [
>   Option ['d'] ["debug"] (NoArg Debug) "produce useless debug.pdf"
>  ]

> main = do
>   args <- getArgs
>   (cliFlags, chart) <- case getOpt RequireOrder cliOptions args of
>       (flags, [filename], []) -> do
>           result <- parseFromFile parseChart filename
>           case result of
>               Left err  -> do
>                   hPutStrLn stderr (show err)
>                   exitWith (ExitFailure 1)
>               Right xs  -> return (flags, xs)
>       (flags, _, errs) -> do
>           let hdr = "Usage: explain-chart [OPTIONS] <filename>"
>           hPutStr stderr (concat errs ++ usageInfo hdr cliOptions)
>           exitWith (ExitFailure 1)

Define some helpers for the command line options processing.

>   let whenFlag flag m = when (flag `elem` cliFlags) m

Figure out the chart dimensions.

>   let (xmin, xmax, xlabeled) = getAxisRange X chart
>   let (ymin, ymax, ylabeled) = getAxisRange Y chart

Decide to plot 1000 points in each dimension unit.
Define the y range limits appropriately, to make the chart at least as
as tall as y-range.

>   let xcoords = [xmin, xmin + (xmax-xmin)/1000 .. (xmax - 0.1)]
>   let ylimits = [[ (LMin, LValue ymin), (LMin, LValue ymax) ]]

Convert intersections as DSL entities into the corresponding shapes' lists
of their intersections.

>   let chart' = pushDownIntersections chart
>   let shapes = collect chart' :: [Shape]

>   let plot_of_shape shape =
>        let ((name, f), _) = reify_shape (xmin,xmax) (ymin,ymax) shape in
>        plot_lines_values ^= [[ (x, f x) | x <- xcoords, f x < ymax, f x > ymin]]
>        $ plot_lines_limit_values ^= ylimits
>        $ plot_lines_title ^= name
>        $ defaultPlotLines
> 
>   whenFlag Debug $
>       mapM_ (\s ->
>           let ((name, f), (_, cfs)) = reify_shape (xmin,xmax) (ymin,ymax) s
>           in plot_cost_functions name ((-10, 10), (-10, 10)) cfs
>           ) shapes

The chart file may include assertions for the coefficient ranges. Check
them there and terminate program if they do not match. We use BangPattern
here in order to flush out the trace messages and make sure the failure
text comes at the very end of the output.

>   maybeErrs <- mapM (\s ->
>       let !((name, f), (final_coeffs, cfs)) = reify_shape (xmin,xmax) (ymin,ymax) s in
>       return $ check_coefficients name chart final_coeffs
>       ) shapes
>   when (not $ null $ catMaybes maybeErrs) $ do
>       hPutStrLn stderr (intercalate "\n" $ catMaybes maybeErrs)
>       exitWith (ExitFailure 2)


>   let plots = zipWith (\c -> plot_lines_style .> line_color ^= opaque c)
>                       (cycle colors)
>                       (map plot_of_shape shapes)
> 
>   let hidden_range = PlotHidden {
>         plot_hidden_x_values_ = [xmin, xmax],
>         plot_hidden_y_values_ = [ymin, ymax]
>       }
>   let layout =
>          layout1_bottom_axis .> laxis_override ^= axisTicksAtLabels . (if xlabeled then id else axisLabelsHide . axisTicksHide)
>          $ layout1_left_axis .> laxis_override ^= axisTicksAtLabels . (if ylabeled then id else axisLabelsHide . axisTicksHide)
>          $ layout1_plots ^= [Left (toPlot p) | p <- plots] ++
>                             [Left (toPlot hidden_range)]
>          $ defaultLayout1
>   mapM_ (renderableToPDFFile (toRenderable layout) 400 400 . show)
>         (collect chart :: [Filename])

Concretize vague shape coefficients by optimizing for intersections,
shape center, and other constraints. Returns a name and the evaluation
function (\x -> f x).

> reify_shape xrange yrange (Shape name shape intersections) =
>   let
>       coeff_guesses = coefficients shape xrange yrange
>       cx = center_x shape xrange
>       cy = center_y shape yrange
>       (degree, cost_functions) = costFunction (cx, cy) coeff_guesses (map sp_xy intersections)
>       cost_f cs = sum $ map (flip snd cs) cost_functions
>       (final_coeffs, _) = trace ("Coefficient guesses for " ++ name ++ ": "
>                                  ++ show coeff_guesses) $
>                           minimize NMSimplex2 1E-5 1000 (replicate degree 20) cost_f (replicate degree 1)
>   in trace ("Final coefficients " ++ show final_coeffs)
>            ((name, evalPoly (poly LE final_coeffs)), (final_coeffs, cost_functions))

> plot_cost_functions name ((xl, xr), (yl, yr)) cfs =
>   let
>       points :: ([Double] -> Double) -> [((Double, Double), Double)]
>       points f =
>           [((a, b), f [a, b])
>               | a <- [xl, xl+(xr-xl)/400 .. xr],
>                 b <- [yl, yl+(yr-yl)/400 .. yr],
>                 f [a, b] < 1]
>       cmpSnd a b = compare (snd a) (snd b)
>       normalize pts =
>           let min = snd $ minimumBy cmpSnd pts
>               max = snd $ maximumBy cmpSnd pts
>               rescale v = (v - min) / (max - min)
>           in map (\(ab, v) -> (ab, rescale v)) pts
>       make_dot shift ((a, b), v) =
>               plot_points_values ^= [(a + shift * 0.01, b)]
>               $ plot_points_style ^= (filledCircles 1 $ withOpacity (rgb 1 (shift) 0) v)
>               $ defaultPlotPoints
>       field shift (name, cost_f) = map (make_dot shift) (normalize $ points cost_f)
>       cost_function =
>           let ((_, f1) : (_, f2) : (_, f3) : []) = cfs
>           in [(undefined, \cs -> f2 cs )]
>       layout  =
>         layout1_plots ^= [Left (toPlot p) | p <- concat $ zipWith field [0..] cost_function]
>         $ layout1_title ^= name ++ (show $ length cfs)
>         $ defaultLayout1
>   in renderableToPDFFile (toRenderable layout) 500 500 "debug.pdf"
>   where sigmoid t = t / (1 + exp(-t))

> axisTicksAtLabels ad@(AxisData { axis_labels_ = al }) = ad {
>   axis_ticks_ = concatMap (\(x, _label) -> [(x, 5),(x, -5)]) $ head al
>  }

> getAxisRange kind chart =
>   let axis_prop_guesses =
>          map (\axis -> (range_min axis, range_max axis, labeled axis))
>          $ filter (\axis -> kind == axis_kind axis)
>          $ (collect chart :: [Axis])
>   in head (axis_prop_guesses ++ [(0, 100, True)])

> colors = [ rgb r g b |
>             r <- [1, 0, 0.5],
>             g <- [0, 1, 0.5],
>             b <- [0.5, 0, 1],
>             r + g + b < 1.7
>          ]

