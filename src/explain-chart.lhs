> {-# LANGUAGE BangPatterns, ViewPatterns #-}
> import System.Console.GetOpt
> import System.Environment (getArgs)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hPutStr, hPutStrLn, stderr)

> import Data.Data
> import Data.List
> import Data.Maybe
> import Data.Monoid
> import Data.Generics
> import Test.QuickCheck hiding (collect)
> import Control.Monad (when, ap)

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
> import ChartModel.Geometry
> import ChartModel.Parser
> import ChartModel.Explain

> import qualified Debug.Trace

> data Flag = Verbose deriving Eq
> cliOptions :: [OptDescr Flag]
> cliOptions = [
>   Option ['v'] ["verbose"] (NoArg Verbose) "dump more debugging info (also -vv)"
>  ]

> main = do
>   args <- getArgs
>   (cliFlags, chart) <- case getOpt Permute cliOptions args of
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
whenFlag executes a monadic computation when a specified flag is set.
(shout Short|Long) is equivalent to Debug.Trace.trace when the appropriate
number of "-v" command line options are specified.

>   let whenFlag flag m = when (flag `elem` cliFlags) m
>   let cliVerbosityLevel = length $ filter (==Verbose) cliFlags
>   let shout = makeLogger cliVerbosityLevel
>   let
>       lappend (needLog cliVerbosityLevel -> True)  x y = x `mappend` y
>       lappend (needLog cliVerbosityLevel -> False) x y = x

Check that the file contains no duplicate definitions.

>   case sort (collectMap name chart) of
>     [] -> fail "No shape definitions found"
>     names -> case map fst $ filter (uncurry (==)) $ (ap zip tail) names of
>               [] -> return ()
>               dups -> fail $ "Duplicate definitions for shapes: "
>                              ++ (intercalate ", "  dups)

Figure out the chart dimensions.

>   let (xrange, xaxis) = getAxis X chart
>   let (yrange, yaxis) = getAxis Y chart

Decide to plot 1000 points in each dimension unit.
Define the y range limits appropriately, to make the chart at least as
as tall as y-range.

>   let xcoords = [fst xrange, fst xrange + (snd xrange - fst xrange)/1000 .. (snd xrange - 0.1)]
>   let ylimits = [[ (LMin, LValue $ fst yrange), (LMin, LValue $ snd yrange) ]]

Convert intersections as DSL entities into the corresponding shapes' lists
of their intersections.

>   let chart' = pushDownIntersections chart
>   let all_shapes = collect chart' :: [Shape]
>   let all_reified_shapes = reify_shapes shout xrange yrange all_shapes

A function to allow us to filter against a non-empty list of elements.
If we don't have a list (that is, a list is empty), we don't filter.

>   let maybeFilter unwrap values f list = case values of
>           [] -> list
>           vs -> filter (\(unwrap -> e) -> f (e `elem` vs)) list

That function is used to filter against "show A, B" and "hide C, D" clauses
which allow control over what gets displayed on the graph.

>   let show_shapes = concatMap sshow (collect chart' :: [ShapeShow])
>   let hide_shapes = concatMap shide (collect chart' :: [ShapeHide])
>   let reified_shapes =
>           let name = fst . fst
>           in maybeFilter name hide_shapes not
>            $ maybeFilter name show_shapes id all_reified_shapes

>   let plot_of_shape ((name, final_coeffs), f) =
>        plot_lines_values ^= [capbox xrange yrange $ map (ap (,) f) xcoords]
>        $ plot_lines_limit_values ^= ylimits
>        $ plot_lines_title ^= lappend Short name (" " ++ showPolynome final_coeffs)
>        $ defaultPlotLines

The chart file may include assertions for the coefficient ranges. Check
them there and terminate program if they do not match. We use BangPattern
here in order to flush out the trace messages and make sure the failure
text comes at the very end of the output.

>   maybeErrs <- mapM (\((name, final_coeffs), f) ->
>       return $ check_coefficients name chart final_coeffs
>       ) reified_shapes
>   when (not $ null $ catMaybes maybeErrs) $ do
>       hPutStrLn stderr (intercalate "\n" $ catMaybes maybeErrs)
>       when (not $ needLog cliVerbosityLevel Short) $ exitWith (ExitFailure 2)


>   let plots = zipWith (\c -> plot_lines_style .> line_color ^= opaque c)
>                       (cycle colors)
>                       (map plot_of_shape reified_shapes)
> 
>   let hidden_range = PlotHidden {
>         plot_hidden_x_values_ = [fst xrange, snd xrange],
>         plot_hidden_y_values_ = [fst yrange, snd yrange]
>       }
>   let layout =
>          layout1_bottom_axis .> laxis_override ^= axisTicksAtLabels . (if labeled xaxis then id else axisLabelsHide . axisTicksHide)
>          $ layout1_left_axis .> laxis_override ^= axisTicksAtLabels . (if labeled yaxis then id else axisLabelsHide . axisTicksHide)
>          $ case title xaxis of Nothing -> id; Just t -> layout1_bottom_axis .> laxis_title ^= t
>          $ case title yaxis of Nothing -> id; Just t -> layout1_left_axis .> laxis_title ^= t
>          $ layout1_plots ^= [Left (toPlot p) | p <- plots] ++
>                             [Left (toPlot hidden_range)]
>          $ layout1_title ^= lappend Short "" " (verbose output)"
>          $ defaultLayout1
> 
>   putStrLn $ explain xrange yrange $ map fst reified_shapes
> 
>   flip mapM_ (collect chart :: [Filename]) $ \file -> do
>       renderableToPDFFile (toRenderable layout) 500 500 (show file)
>       putStrLn ("Image saved as " ++ show file)

Concretize vague shape coefficients by globally optimizing for intersections,
shape center, and other constraints.

> reify_shapes shout xrange yrange all_shapes =
>   let (numcs, shapes_and_positions, costFunction, name2eval) =
>           combinedShapesCost xrange yrange all_shapes
>       cost_f = sum . map snd . costFunction
>       polyForms = map (fromPolyForm . shape) $ filter (isPolyForm . shape) all_shapes
>       coeff_constraints = concatMap (\p -> coefficients p xrange yrange) polyForms
>       coeff_init_guess = concatMap (\p -> coeff_initial_guess p xrange yrange) polyForms
>       sbox = concatMap (\p -> search_box p xrange yrange) polyForms
>       !(final_coeffs, p) =
>           minimize NMSimplex2 1E-5 1000 sbox cost_f coeff_init_guess
>   in Debug.Trace.trace ("Positions: " ++ (show $ coefficientPositions all_shapes)) $ map (\(s, span) ->
>       let n = name s
>           cs = select span final_coeffs
>           !_ = shape_costf xrange yrange s
>       in shout Short ("NEW " ++ n ++ ":" ++ show span)
>          $ shout Short ("  N Intersections " ++ show (shape_intersections s))
>          $ shout Short ("  N Minimize search box: " ++ show (select span sbox))
>          $ shout Short ("  N Constraint coeffs: " ++ show (select span coeff_constraints))
>          $ shout Short ("  N Guess coefficients " ++ show (select span coeff_init_guess))
>          $ shout Short ("  N Final coefficients " ++ show cs)
>          ((n, cs), name2eval final_coeffs n)
>   ) shapes_and_positions
>   where select (start, span) = reverse . take span . drop start . reverse


> shape_costf xrange yrange (Shape name shape@(DerivedForm _) intersections) = []
> shape_costf xrange yrange (Shape name shape@(PolyForm _) intersections) =
>   let
>       coeff_constraints = (coefficients . fromPolyForm) shape xrange yrange
>       coeff_init_guess = (coeff_initial_guess . fromPolyForm) shape xrange yrange
>       cx = center_x shape xrange
>       cy = center_y shape yrange
>       cost_functions = polyCostFunction (cx, cy) coeff_constraints (map sp_xy intersections)
>       cost_f cs = sum $ map (flip snd cs) cost_functions
>       sbox = (search_box . fromPolyForm) shape xrange yrange
>       !(final_coeffs, p) =
>           minimize NMSimplex2 1E-5 100 sbox cost_f coeff_init_guess
>   in Debug.Trace.trace ("OLD " ++ name ++ ":")
>      $ Debug.Trace.trace ("  O Intersections " ++ show intersections)
>      $ Debug.Trace.trace ("  O Minimize search box: " ++ show sbox)
>      $ Debug.Trace.trace ("  O Constraint coeffs: " ++ show coeff_constraints)
>      $ Debug.Trace.trace ("  O Guess coefficients " ++ show coeff_init_guess)
>      $ Debug.Trace.trace ("  O Final coefficients " ++ show final_coeffs)
>      final_coeffs


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
>   axis_ticks_ = if null al then [] else concatMap (\(x, _label) -> [(x, 5),(x, -5)]) $ head al
>  }


Extract the given kind of axis from the chart definition, or return the default.

> getAxis :: AxisKind -> Chart -> ( (Double, Double), Axis )
> getAxis kind chart =
>   let axis_prop_guesses =
>          map extract
>          $ filter (\axis -> kind == axis_kind axis)
>          $ collect chart
>   in head (axis_prop_guesses ++ [extract $ defaultAxis kind])
>   where extract axis = ((range_min axis, range_max axis), axis)

Produce a list of distinctive colors. The colors should not be too bright,
so we limit the total brightness to 1.7 out of 3.

> colors = [ rgb r g b |
>             r <- [1, 0, 0.5],
>             g <- [0, 1, 0.5],
>             b <- [0.5, 0, 1],
>             r + g + b < 1.7
>          ]

Create a helper trace function depending on the number of "-v"'s given
in the command line. VerbosityLevel is really a log level.

> data Verbosity = Short | Long deriving Enum
> makeLogger cliLevel (needLog cliLevel -> True) = Debug.Trace.trace
> makeLogger cliLevel (needLog cliLevel -> False) = flip const
> needLog cliLevel verbosity = cliLevel > fromEnum verbosity

