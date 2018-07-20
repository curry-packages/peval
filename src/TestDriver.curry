--- ----------------------------------------------------------------------------
--- This program runs some test cases for the partial evaluator
--- using the different semantics and abstractions available.
---
--- The result of partial evaluation (specialized program) is compared to the
--- expected file `<test>.spec` for equality. Since abstraction based on
--- a well-founded ordering (wfo) or well-quasi ordering (wqo) may lead
--- to different results, in those cases there also exist files `<test>.wfo`
--- and `<test>.wqo`. Furthermore, some examples may not terminate without
--- abstraction, indicated by an empty `<test>.timeout` file.
---
--- In addition to the comparison of the computed and expected specialization,
--- both the original and specialized program can be executed (i.e, the
--- function `main` is evaluated) and the exit codes and computed results
--- are then compared for equality.
---
--- @author  Björn Peemöller
--- @version September 2015
--- ----------------------------------------------------------------------------
module TestDriver where

import System.Console.ANSI.Codes (green, magenta, red, yellow)
import Data.Char                 (toLower, toUpper)
import Data.List                 (intercalate, last, maximum, nub, sortBy)
import System.Directory          ( getDirectoryContents, doesDirectoryExist
                                 , doesFileExist )
import System.FilePath           ( FilePath, (</>), (<.>), dropExtension
                                 , takeBaseName, takeDirectory, takeExtension )
import System.IO                 ( hFlush, hIsTerminalDevice, hPutStrLn
                                 , stderr, stdout )
import System.Process            (exitWith, getArgs, getProgName)
import IOExts                    (evalCmd)

import GetOpt2                   ( OptDescr (..), ArgDescr (..)
                                 , ArgOrder (Permute), OptErr, OptTable
                                 , getOpt, onOpts, option, usageInfo )
import Utils                     ((+\+), count, countBy, indentStr, lpad, rpad)
import Configuration

-- -----------------------------------------------------------------------------
-- Main
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  opts  <- getOpts
  tests <- getTestPrograms opts
  runTests opts tests

-- -----------------------------------------------------------------------------
-- Constants, Options and Parsing of Command-Line Arguments
-- -----------------------------------------------------------------------------

--- Path to partial evaluator
pevalCmd :: String
pevalCmd = "./peval"

--- Path to timeout command
timeoutCmd :: String
timeoutCmd = "/usr/bin/timeout"

--- Options supplied to PAKCS for evaluation of program
pakcsEvalOpts :: String -> [String]
pakcsEvalOpts mod = [":set", "v0", ":set", "-time", ":set", "parser", "-Wnone"
                    , ":load", mod, ":eval", "main", ":quit"]

--- Timeout used for evaluation of program using PAKCS
evalTimeout :: Int
evalTimeout = 5

--- Accepted return codes of evaluation of some program
--- (0 = okay, 2 = no value found, 124 = time out)
evalReturnCodes :: [Int]
evalReturnCodes = [ 0, 2, 124 ]

--- Options of test driver.
--- @field optHelp      - show the help and exit
--- @field optVerbose   - Enable verbose mode, showing error messages and
---                       computed/expected program in case of differences
--- @field optColorMode - mode for output coloring
--- @field optTestDir   - directory to search for test cases
--- @field optTimeout   - timeout in seconds for partial evaluation
--- @field optSemantics - semantics to be tested
--- @field optAbstract  - abstraction operators to be tested
--- @field optEval      - Enable evaluation of original/specialized program
--- @field optAssert    - Run partial evaluation with assertions enabled
--- @field optSuffix    - suffix for specialized programs
--- @field optPakcsCmd  - Path to the PAKCS binary, used for evaluation
data Options = Options
  { optHelp      :: Bool
  , optVerbose   :: Bool
  , optColorMode :: ColorMode
  , optTestDir   :: FilePath
  , optTimeout   :: Int
  , optSemantics :: [Semantics]
  , optAbstract  :: [Abstraction]
  , optEval      :: Bool
  , optAssert    :: Bool
  , optSuffix    :: String
  , optPakcsCmd  :: String
  }

--- The default options
defaultOptions :: Options
defaultOptions = Options
  { optHelp      = False
  , optVerbose   = False
  , optColorMode = CMAuto
  , optTestDir   = "test"
  , optTimeout   = 2
  , optSemantics = []
  , optAbstract  = []
  , optEval      = False
  , optAssert    = False
  , optSuffix    = "_pe"
  , optPakcsCmd  = "/opt/pakcs/pakcs/bin/pakcs"
  }

--- Option descriptions
options :: [OptDescr (OptErr Options -> OptErr Options)]
options =
  [ Option ['h', '?'] ["help"      ]
      (NoArg (onOpts $ \o -> o { optHelp = True }))
      "show usage information"
  , Option ['v']      ["verbose"]
      (NoArg (onOpts $ \o -> o { optVerbose = True }))
      "run in verbose mode"
  , Option ['d'] ["testdir"]
      (ReqArg (\d -> onOpts $ \o -> o { optTestDir = d }) "DIR")
      "search for test cases in DIR"
  , Option ['t'] ["timeout"]
      (ReqArg (\t -> onOpts $ \o -> o { optTimeout = readInt t }) "N")
      "set timeout for partial evaluation to N seconds"
  , Option ['e'] ["eval"]
      (NoArg (onOpts $ \o -> o { optEval = True }))
      "evaluate original and specialized program using PAKCS (slower)"
  , Option []    ["assert"]
      (NoArg (onOpts $ \o -> o { optAssert = True }))
      "check additional abstraction assertions (slower)"
  , Option []    ["suffix"]
      (ReqArg (\c -> onOpts $ \o -> o { optSuffix = c }) "SUFFIX")
      "specify the suffix for the specialised program"
  , Option []    ["pakcs"]
      (ReqArg (\c -> onOpts $ \o -> o { optPakcsCmd = c }) "PAKCSCMD")
      "specify the path to the PAKCS curry system"
  , option []         ["color", "colour"] colorDescriptions     "mode"
      "coloring mode"
  , option ['S']      ["semantics" ]      semanticsDescriptions "semantics"
      "semantics for evaluation"
  , option ['A']      ["abstract"  ]      abstractDescriptions  "mode"
      "abstraction mode"
  ]

--- Semantics descriptions
semanticsDescriptions :: OptTable Options
semanticsDescriptions = map toDescr semantics
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set s opts = opts { optSemantics = nub (optSemantics opts ++ [s]) }

--- Abstraction descriptions
abstractDescriptions :: OptTable Options
abstractDescriptions = map toDescr abstractions
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set a opts = opts { optAbstract = nub (optAbstract opts ++ [a]) }

--- ColorMode descriptions
colorDescriptions :: OptTable Options
colorDescriptions = map toDescr colorModes
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set c opts = opts { optColorMode = c }

--- Parse the supplied command-line arguments.
parseOpts :: [String] -> (Options, [String], [String])
parseOpts args = (opts, files, errs ++ argErrs)
  where
  (opts, argErrs)        = foldl (flip ($)) (defaultOptions, []) optErrs
  (optErrs, files, errs) = getOpt Permute options args

--- Print the usage information and exit.
printUsage :: String -> IO a
printUsage prog = do
  flushStrLn $ usageInfo header options
  exitWith 0
    where header = "usage: " ++ prog ++ " [OPTION] ... MODULE ..."

--- Print errors about bad usage and exit with error code 1.
badUsage :: String -> [String] -> IO a
badUsage prog []         = do
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitWith 1
badUsage prog (err:errs) = hPutStrLn stderr err >> badUsage prog errs

--- Process the parsed options, eventually showing help or errors
processOpts :: String -> (Options, [String], [String])
            -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | optHelp opts     = printUsage prog
  | not (null errs') = badUsage prog errs'
  | otherwise        = do
    opts' <- setColoring opts
    return (opts', files)
  where errs' = errs ++ checkOpts opts files

--- Check the parsed options for validity
checkOpts :: Options -> [String] -> [String]
checkOpts opts _ = [ "Please specify a semantics"    | null (optSemantics opts)]
                ++ [ "Please specify an abstraction" | null (optAbstract  opts)]

--- Set the coloring mode depending on the connected output stream.
setColoring :: Options -> IO Options
setColoring opts
  | optColorMode opts == CMAuto = do
    isTerminal <- hIsTerminalDevice stdout
    return opts { optColorMode = if isTerminal then CMAlways else CMNever }
  | otherwise   = return opts

--- Retrieve, parse and validate the program options
getOpts :: IO Options
getOpts = do
  args <- getArgs
  prog <- getProgName
  (opts, _) <- processOpts prog $ parseOpts args
  return opts

-- -----------------------------------------------------------------------------
-- Data Types and Functions for Test Cases
-- -----------------------------------------------------------------------------

--- Type synonym for a test case (name, test module, test spec files)
type TestCase = (String, FilePath, [FilePath])

--- Retrieve the name of a test case.
getName :: TestCase -> String
getName (n, _, _) = n

--- Retrieve the length of the longest name of a list of test cases.
maxNameLength :: [TestCase] -> Int
maxNameLength tests = maximum $ 0 : map (length . getName) tests

--- Test result in increasing severity.
--- @cons Ok       : Expected result, both for partial evaluation and program
---                  execution.
--- @cons Custom   : Expected result for a specific abstraction and expected
---                  result for program execution.
--- @cons Timeout  : Partial evaluation did not finish in time.
---                  Acceptable only for no abstraction mode.
--- @cons Failed   : The partial evaluator returned an unexpected result,
---                  but the program semantics is as expected.
--- @cons NoEval   : The original program could not be executed. This
---                  decreases the significance of the test as the semantic
---                  equivalence can not be tested.
--- @cons EvalError: The resulting program has a different semantics compared
---                  to the original program. This is really bad!
--- @cons Error    : The partial evaluation aborted with an internal error.
data Result = Ok | Custom | Timeout | Failed | NoEval | EvalError | Error
  deriving (Eq,Show)

--- Colorize a `String` with a given coloring function,
--- depending on the global coloring mode.
color :: Options -> (String -> String) -> String -> String
color opts paint s = if optColorMode opts == CMAlways then paint s else s

--- Show the result of a test case.
showResult :: Options -> Result -> String
showResult opts res = color opts (resColor res) (map toUpper $ show res)
  where
  resColor Ok        = green
  resColor Custom    = green
  resColor Timeout   = magenta
  resColor Failed    = yellow
  resColor NoEval    = yellow
  resColor EvalError = red
  resColor Error     = red

-- -----------------------------------------------------------------------------
-- Running Tests
-- -----------------------------------------------------------------------------

--- Retrieve all available test cases.
getTestPrograms :: Options -> IO [TestCase]
getTestPrograms opts = do
  mdls  <- findFilesRec (hasExtension (== ".curry")) (optTestDir opts)
  concat `liftIO` mapIO (getOutputSpecs opts) mdls

--- Get the output specification for a specific module.
getOutputSpecs :: Options -> FilePath -> IO [TestCase]
getOutputSpecs opts m = do
  let d = takeDirectory m
  specs <- findFiles (isSpecOf opts m) d
  return $ if null specs then [] else [(dropExtension m, m, map (d </>) specs)]

--- Is a file a specification file of the given test module?
isSpecOf :: Options -> FilePath -> FilePath -> Bool
isSpecOf opts m s = takeBaseName s == takeBaseName m
                 && hasExtension
                    (`elem` ".spec" : map absSpec (optAbstract opts)) s
  where absSpec abs = '.' : toOption abs

--- Run all given test cases for all specified abstractions and semantics.
runTests :: Options -> [TestCase] -> IO ()
runTests opts tests = do
  results <- mapIO (\(sem, abs) -> testSemAbs opts len sem abs tests) combs
  flushStrLn $ replicate len '='
  flushStrLn headers
  flushStrLn $ replicate len '-'
  forIO (zip combs results) $ \ ((sem, abs), res) -> do
    flushStrLn $ intercalate "  "
      [ rpad  9 (show sem)
      , rpad 11 (show abs)
      , lpad  6 (show $ length tests)
      , color opts green   $ lpad  3 $ show (count Ok        res)
      , color opts green   $ lpad  7 $ show (count Custom    res)
      , color opts magenta $ lpad  8 $ show (count Timeout   res)
      , color opts yellow  $ lpad  7 $ show (count Failed    res)
      , color opts yellow  $ lpad  7 $ show (count NoEval    res)
      , color opts red     $ lpad 10 $ show (count EvalError res)
      , color opts red     $ lpad  6 $ show (count Error     res)
      ]
  flushStrLn $ replicate len '='
  unless (all (`elem` [Ok, Custom, Timeout]) (concat results)) (exitWith 1)
 where
  headers = intercalate "  "
            ["Semantics", "Abstraction", "#Tests", "#Ok", "#Custom", "#Timeout"
            , "#Failed", "#NoEval", "#EvalError", "#Error"]
  len     = length headers
  combs   = [(s, a) | s <- optSemantics opts, a <- optAbstract opts]

--- Run all given test cases for a specific combination of semantics
--- and abstraction operator.
testSemAbs :: Options -> Int -> Semantics -> Abstraction -> [TestCase]
           -> IO [Result]
testSemAbs opts len sem abs tests = do
  flushStrLn $ replicate len '='
  flushStrLn $ "Testing " ++ color opts yellow (show   sem) ++ " semantics "
             ++ "and "    ++ color opts yellow (show   abs) ++ " abstraction "
             ++ "with "   ++ color opts yellow (show total) ++ " test(s)."
  flushStrLn $ replicate len '-'
  results <- mapIO (runTest opts testLen sem abs) tests
  let okCount = countBy (`elem` [Ok, Custom]) results
      toCount = count Timeout results
      flCount = countBy (`elem` [Failed, NoEval]) results
      erCount = countBy (`elem` [EvalError, Error]) results
  flushStrLn $ replicate len '='
  flushStrLn $ show total ++ " test(s) executed, "
          ++ color opts green   (show okCount) ++ " ok, "
          ++ color opts magenta (show toCount) ++ " timeouts, "
          ++ color opts yellow  (show flCount) ++ " failed, "
          ++ color opts red     (show erCount) ++ " errors.\n"
  return results
 where
  total   = length tests
  testLen = maxNameLength tests

--- Run a single test case for a specific combination of semantics
--- and abstraction operator.
runTest :: Options -> Int -> Semantics -> Abstraction -> TestCase -> IO Result
runTest opts len sem abs (name, mod, specs) = do
  let pCall = [ pevalCmd
              , "-v0", "--debug"
              , "--semantics=" ++ toOption sem
              , "--abstract="  ++ toOption abs
              , "--suffix="    ++ optSuffix opts
              , mod ] ++ [ "--assert" | optAssert opts ]
  flushStr $ "Testing " ++ rpad len name ++ ": "
  (ec, res, err) <- evalCmd timeoutCmd (show (optTimeout opts) : pCall) []
  spcs <- mapIO readFile
    (filter (hasExtension (`elem` [".spec", "." ++ toOption abs])) specs)
  result <- case ec of
    0   | res == head spcs -> evalTest opts (dropExtension mod) Ok
        | res  `elem` spcs -> evalTest opts (dropExtension mod) Custom
        | otherwise        -> evalTest opts (dropExtension mod) Failed
    124 | abs == None      -> doesFileExist (name <.> "timeout") >>= \exists ->
                              return (if exists then Timeout else Failed)
        | otherwise        -> return Failed
    _                      -> return Error
  flushStrLn $ showResult opts result
  when (result == Failed && optVerbose opts) $ do
    flushStrLn $ color opts yellow $ "Expected:" +\+ indentStr 2 (last spcs)
    flushStrLn $ color opts yellow $ "Got:"      +\+ indentStr 2 res
  when (result == Error  && optVerbose opts) $ do
    flushStrLn $ color opts red $ "Error message:" +\+ indentStr 2 err
  return result

--- Convert a semantics/abstraction to an option name.
toOption :: Show a => a -> String
toOption = map toLower . show

--- Evaluate a original program and its specialization and compare the results.
evalTest :: Options -> String -> Result -> IO Result
evalTest opts mod def
  | optEval opts = do
    (ec1, res1, err1) <- eval opts mod
    if ec1 `notElem` evalReturnCodes
      then print (ec1, res1, err1) >> return NoEval
      else do
        (ec2, res2, err2) <- eval opts (mod ++ optSuffix opts)
        if (ec1 == ec2 && res1 == res2 && err1 == err2)
            -- PAKCS does no blackhole detection!
            || mod == "test/base/blackhole"
          then return def
          else print ((ec1,ec2), (res1,res2), (err1,err2)) >> return EvalError
  | otherwise    = return def

--- Evaluate a Curry program using PAKCS (with a given timeout)
--- and return its exit code as well as the contents of `stdout` and `stderr`.
eval :: Options -> String -> IO (Int, String, String)
eval opts mod = evalCmd timeoutCmd args []
  where args = show evalTimeout : optPakcsCmd opts : pakcsEvalOpts mod

-- -----------------------------------------------------------------------------
-- Directory lookup and strict output
-- -----------------------------------------------------------------------------

--- Check if the extension of a `FilePath` satisfies the given predicate.
hasExtension :: (String -> Bool) -> FilePath -> Bool
hasExtension p fn = p (takeExtension fn)

--- Find all files in the given directory that satisfy the given predicate.
findFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFiles p dir = filter p `liftIO` getUsefulContents dir

--- Recursively find all files in the given directory and any subdirectories
--- that satisfy the given predicate.
findFilesRec :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFilesRec p dir = filter p `liftIO` getRecursiveContents dir

--- Recursively find all files in the given directory.
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getUsefulContents topdir
  paths <- forIO names $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

--- List all files and directores in the given directory,
--- excluding the `.curry` directory.
getUsefulContents :: FilePath -> IO [String]
getUsefulContents dir = (sortBy (<=) . filter (`notElem` [".", "..", ".curry"]))
                        `liftIO` getDirectoryContents dir

--- Print a `String` to `stdout` and directly flush the stream afterwards
--- to provide immediate information to the user.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

--- Print a `String` followed by a newline to `stdout` and directly flush
--- the stream afterwards to provide immediate information to the user.
flushStrLn :: String -> IO ()
flushStrLn str = putStrLn str >> hFlush stdout
