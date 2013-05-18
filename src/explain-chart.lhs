> {-# LANGUAGE BangPatterns, ViewPatterns, TupleSections #-}
> import System.Console.GetOpt
> import System.Environment (getArgs)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hPutStr, hPutStrLn, stdout, stderr)

> import Data.Data
> import Data.List
> import Data.Maybe
> import Data.Monoid
> import Data.Generics
> import Test.QuickCheck hiding (collect)
> import Control.Monad (when, ap, forM_)

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

Read in the chart DSL from file, and do a duplication check.

> chartFromFile :: String -> IO Chart
> chartFromFile filename = do
>   result <- parseFromFile parseChart filename
>   chart <- case result of
>               Left err  -> do
>                   hPutStrLn stderr (show err)
>                   exitWith (ExitFailure 1)
>               Right xs -> return xs
>   return chart

Extract the shape descriptions from the chart, compute the shape parameters
according to chart constraints and return a list of shapes. 

> shapesFromChart chart =
>   let (xrange, _xaxis) = getAxis X chart
>       (yrange, _yaxis) = getAxis Y chart
>       chart' = pushDownIntersections chart  -- Copy intersections into shapes
>       shapes = collect chart' :: [Shape]
>   in reify_shapes xrange yrange shapes

Extract a certain shape by name from the given file and show details
of conversion from the fuzzy shape into a concrete polynome.

This function can be nicely used with GHCi:

    sh> ghci src/explain-chart.lhs
    ghci> shapeFromFile "test/test1.chart" "NegCentered"

> shapeFromFile :: String -> String -> IO ()
> shapeFromFile filename shapename = do
>   chart <- chartFromFile filename
>   (_, shapes) <- return $ shapesFromChart chart
>   Just (_, _f, printout) <- return $ find (\((n, _), _, _) -> n == shapename) shapes
>   printout


A structure to define and capture the command line options.

> data Flag = Verbose deriving Eq
> cliOptions :: [OptDescr Flag]
> cliOptions = [
>   Option ['v'] ["verbose"] (NoArg Verbose) "dump more debugging info (also -vv)"
>  ]


int main() { return 0; };

> main = do
>   args <- getArgs
>   (cliFlags, chart) <- case getOpt Permute cliOptions args of
>       (flags, [filename], []) -> fmap (flags,) $ chartFromFile filename
>       (flags, _, errs) -> do
>           let hdr = "Usage: explain-chart [OPTIONS] <filename>"
>           hPutStr stderr (concat errs ++ usageInfo hdr cliOptions)
>           exitWith (ExitFailure 1)

>   checkShapeDuplicates chart

Define some helpers for the command line options processing.
whenFlag executes a monadic computation when a specified flag is set.
(shout Short|Long) is equivalent to Debug.Trace.trace when the appropriate
number of "-v" command line options are specified.

>   let whenFlag flag m = when (flag `elem` cliFlags) m
>   let cliVerbosityLevel = length $ filter (==Verbose) cliFlags
>   let
>       lappend (needLog cliVerbosityLevel -> True)  x y = x `mappend` y
>       lappend (needLog cliVerbosityLevel -> False) x y = x

Figure out the chart dimensions.

>   let (xrange, xaxis) = getAxis X chart
>   let (yrange, yaxis) = getAxis Y chart

Decide to plot 1000 points in each dimension unit.
Define the y range limits appropriately, to make the chart at least as
as tall as y-range.

>   let xcoords = [fst xrange, fst xrange + (snd xrange - fst xrange)/1000 .. (snd xrange - 0.1)]
>   let ylimits = [[ (LMin, LValue $ fst yrange), (LMin, LValue $ snd yrange) ]]

A function to allow us to filter against a non-empty list of elements.
If we don't have a list (that is, a list is empty), we don't filter.

>   let maybeFilter unwrap values f list = case values of
>           [] -> list
>           vs -> filter (\(unwrap -> e) -> f (e `elem` vs)) list

That function is used to filter against "show A, B" and "hide C, D" clauses
which allow control over what gets displayed on the graph.

>   let show_shapes = concatMap sshow (collect chart :: [ShapeShow])
>   let hide_shapes = concatMap shide (collect chart :: [ShapeHide])
>   let (_, all_reified_shapes) = shapesFromChart chart
>   let reified_shapes =
>           let name = fst . (\(a, _, _) -> a)
>           in maybeFilter name hide_shapes not
>            $ maybeFilter name show_shapes id all_reified_shapes

>   let plot_of_shape ((name, final_coeffs), f, _printout) =
>        plot_lines_values ^= [capbox xrange yrange $ map (ap (,) f) xcoords]
>        $ plot_lines_limit_values ^= ylimits
>        $ plot_lines_title ^= lappend Short name (" " ++ showPolynome final_coeffs)
>        $ defaultPlotLines

The chart file may include assertions for the coefficient ranges. Check
them there and terminate program if they do not match. We use BangPattern
here in order to flush out the trace messages and make sure the failure
text comes at the very end of the output.

>   maybeErrs <- mapM (\((name, final_coeffs), f, _printout) ->
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
>   putStrLn $ explain xrange yrange $ map (\(a, b, c) -> a) reified_shapes
> 
>   flip mapM_ (collect chart :: [Filename]) $ \file -> do
>       renderableToPDFFile (toRenderable layout) 500 500 (show file)
>       putStrLn ("Image saved as " ++ show file)

>  where

Check that the chart contains no duplicate shape names.

>  checkShapeDuplicates chart =
>   case sort (collectMap name chart) of
>     [] -> fail "No shape definitions found"
>     names -> case map fst $ filter (uncurry (==)) $ (ap zip tail) names of
>               [] -> return ()
>               dups -> fail $ "Duplicate definitions for shapes: "
>                              ++ (intercalate ", "  dups)

Create a certain type of ticks at axis labelsn in the chart.
Purely an aesthetical thing.

>  axisTicksAtLabels ad@(AxisData { axis_labels_ = al }) = ad {
>       axis_ticks_ = if null al then [] else concatMap (\(x, _label) -> [(x, 5),(x, -5)]) $ head al
>    }


Concretize fuzzy shape coefficients by globally optimizing for intersections,
shape center, and other constraints.

> reify_shapes xrange yrange all_shapes =
>   let (numcs, shapes_and_positions, costFunction, name2eval) =
>           combinedShapesCost xrange yrange all_shapes
>       cost_f acs = sum $ concatMap cost_parts $ costFunction (AP acs)
>       polyForms = map fromPolyForm $ filter isPolyForm $ map (shape . fst) shapes_and_positions
>       coeff_constraints = concatMap (\p -> coefficients p xrange yrange) polyForms
>       coeff_init_guess = concatMap (\p -> coeff_initial_guess p xrange yrange) polyForms
>       sbox = concatMap (\p -> search_box p xrange yrange) polyForms
>       !(final_all_coeffs, search_path) =
>           minimize NMSimplex2 1E-10 10000 sbox cost_f coeff_init_guess
>           --minimizeD VectorBFGS2 1E-10 10000 1.0 0.01 cost_f (grad cost_f) (map (+0) coeff_init_guess)
>           --minimizeD ConjugateFR 1E-1 10000 1.0 0.01 cost_f (grad cost_f) (map (+0) coeff_init_guess)
>       p = hPutStrLn stdout
>       general_printout = do
>           --p ("\n\np : " ++ (show search_path))
>           p ("Constraints: " ++ (show $ coeff_constraints))
>           p ("Minimize coeffs: " ++ (show $ sbox))
>           p ("Guesses: " ++ (show $ coeff_init_guess))
>           p ("Final coeffs: " ++ (show $ final_all_coeffs))
>           p ("Forms: " ++ (show $ map (\(s, span) -> (name s, span)) shapes_and_positions))
>    in (general_printout, map (\(s, span) ->
>       let n = name s
>           cs = select span final_all_coeffs
>           specific_printout = do
>               p (n ++ ":" ++ show span)
>               p ("  Intersections " ++ show (shape_intersections s))
>               p ("  Minimize search box: " ++ show (select span sbox))
>               p ("  Constraint coeffs: " ++ show (select span coeff_constraints))
>               p ("  Guess coefficients " ++ show (select span coeff_init_guess))
>               p ("  Final coefficients " ++ showPolynome cs)
>               p ("  Gradient " ++ show (select span (grad cost_f final_all_coeffs)))
>               p ("  " ++ show (fmap ($ select span final_all_coeffs) $ polyCostFunction (50, 50) (coefficients (fromPolyForm $ shape s) xrange yrange) (map sp_xy $ shape_intersections s)))
>               p ("  Cost: " ++ show (sumCost ($ select span final_all_coeffs) $ polyCostFunction (50, 50) (coefficients (fromPolyForm $ shape s) xrange yrange) (map sp_xy $ shape_intersections s)))
>           in ((n, cs), name2eval (AP final_all_coeffs) n, specific_printout)
>   ) shapes_and_positions)
>   where select (start, span) = reverse . take span . drop start . reverse

Numerically compute a gradient for a given polynome. Used for debugging
or as an input into ConjugateFR minimization method.

> grad :: ([Double] -> Double) -> [Double] -> [Double]
> grad cost_f cs =
>   case [ new_cs
>       | epsilon <- [ 10 ** e | e <- [-10 .. -5] ],
>         new_cs <- [[ (cost_f (fmapN n (+epsilon) cs) - cost_f cs) / epsilon
>                      | n <- [0 .. (length cs) - 1] ]],
>         sum new_cs /= 0] of
>       [] -> replicate (length cs) 0
>       (xs:_) -> xs
>   where fmapN n f list =
>           zipWith (\i e -> if i == n then f e else e) [0..] list

A function used for debug. Inconsequential.

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

A helper that displays all of the given functions within the specified range.

    Usage:
    ghci> display_f 0 100 [id]

> display_f :: Double -> Double -> [Double -> Double] -> IO ()
> display_f start stop fs =
>   let plot =
>        plot_lines_values ^= [map (ap (,) f) [start,(start + 0.001 * (stop-start))..stop] | f <- fs]
>        $ defaultPlotLines
>       layout =
>        layout1_plots ^= [Left (toPlot plot)] $ defaultLayout1
>   in renderableToPDFFile (toRenderable layout) 500 500 "debug.pdf"

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

