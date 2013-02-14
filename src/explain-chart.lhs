> import System.Console.GetOpt (getOpt, ArgOrder(..))
> import System.Environment (getArgs)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hPutStrLn, stderr)

> import Data.Data
> import Data.Maybe
> import Data.Generics

The chart is described by some DSL. It is useful to visually
check what that DSL describes. We employ Chart library to draw the graphs.

> import Graphics.Rendering.Chart.Simple
> import Graphics.Rendering.Chart.Renderable
> import Graphics.Rendering.Chart
> import Data.Accessor
> import Data.Colour
> import Data.Colour.SRGB.Linear

We use numeric minimization to compute the optimal shape of the graph,
given various constraints. Constraints may include a list of proposed
intersections, a desired center of the graph, and so on.

> import Numeric.GSL.Minimization
> import Math.Polynomial

> import ChartModel.Primitives
> import ChartModel.Parser

> import Debug.Trace

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
>   print $ (collect chart' :: [Shape])

>   let plot_of_shape shape =
>        let (name, _, f) = reify_shape shape (xmin,xmax) (ymin,ymax) in
>        plot_lines_values ^= [[ (x, f x) | x <- xcoords, f x < ymax, f x > ymin]]
>        $ plot_lines_limit_values ^= ylimits
>        $ plot_lines_title ^= name
>        $ defaultPlotLines

>   let plots = zipWith (\c -> plot_lines_style .> line_color ^= opaque c)
>                 (cycle colors)
>                 (map plot_of_shape $ (collect chart' :: [Shape]))
> 
>   let layout =
>          layout1_bottom_axis .> laxis_override ^= axisTicksAtLabels . (if xlabeled then id else axisLabelsHide . axisTicksHide)
>          $ layout1_left_axis .> laxis_override ^= axisTicksAtLabels . (if ylabeled then id else axisLabelsHide . axisTicksHide)
>          $ layout1_plots ^= [Left (toPlot p) | p <- plots]
>          $ defaultLayout1
>   mapM_ (renderableToPDFFile (toRenderable layout) 500 500 . show)
>         (collect chart :: [Filename])

Concretize vague shape coefficients by optimizing for intersections,
shape center, and other constraints. Returns a name and the evaluation
function (\x -> f x).

> reify_shape (Shape name shape intersections) xrange yrange =
>   let
>       coeff_guesses = coefficients shape xrange yrange
>       cx = center_x shape xrange
>       cy = center_y shape yrange
>       (degree, cost_f) = costFunction (cx, cy) coeff_guesses (map sp_xy intersections)
>       (final_coeffs, _) = trace ("Coefficient guesses for " ++ name ++ ": " ++ show coeff_guesses) $
>                           minimize NMSimplex2 1E-5 1000 (replicate degree 10) cost_f (replicate degree 1)
>   in trace ("Final coefficients " ++ show final_coeffs)
>            (name, final_coeffs, evalPoly (poly LE final_coeffs))

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

