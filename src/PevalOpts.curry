--- --------------------------------------------------------------------------
--- Command line options for the partial evaluator.
---
--- @author  Björn Peemöller
--- @version April 2015
--- --------------------------------------------------------------------------
module PevalOpts
  ( Options (..), defaultOptions, Verbosity (..), Abstraction (..)
  , Semantics (..), ProceedMode (..), ColorMode (..), getOpts
  ) where

import IO            (hIsTerminalDevice, hPutStrLn, stderr, stdout)
import ReadShowTerm  (readsQTerm)
import System        (exitWith, getArgs, getProgName)

import GetOpt2       ( OptDescr (..), ArgDescr (..), ArgOrder (Permute), OptErr
                     , OptTable, addErr, getOpt, onOpts, option, usageInfo)
import Utils         (rpad)

import Configuration

--- Version information of the partial evaluator.
version :: String
version = unlines
  [ "Curry Partial Evaluator"
  , "Version 0.1 of 12/09/2016"
  , "CAU Kiel"
  ]

--- Options to the partial evaluator.
data Options = Options
  { optHelp        :: Bool
  , optVersion     :: Bool
  , optDebug       :: Bool
  , optAssert      :: Bool
  , optClosed      :: Bool
  , optFunPats     :: Bool
  , optVerbosity   :: Verbosity
  , optColorMode   :: ColorMode
  , optSemantics   :: Semantics
  , optAbstract    :: Abstraction
  , optProceedMode :: ProceedMode
  , optSuffix      :: String
  }

--- the default options.
defaultOptions :: Options
defaultOptions = Options
  { optHelp        = False
  , optVersion     = False
  , optDebug       = False
  , optAssert      = False
  , optClosed      = False
  , optFunPats     = True
  , optVerbosity   = Status
  , optColorMode   = CMAuto
  , optSemantics   = Natural
  , optAbstract    = None
  , optProceedMode = PMOne
  , optSuffix      = "_pe"
  }

--- Verbosity level.
data Verbosity = Quiet | Status | Info | Detail | Debug

--- Description and flag of verbosities
verbosities :: [(Verbosity, String, String)]
verbosities = [ (Quiet , "0", "quiet"       )
              , (Status, "1", "show status" )
              , (Info  , "2", "show evaluation and abstraction")
              , (Detail, "3", "show details of evaluation")
              , (Debug , "4", "show information useful for debugging")
              ]

-- -----------------------------------------------------------------------------
-- Specification of command line options.
-- -----------------------------------------------------------------------------

--- Description of the available options.
options :: [OptDescr (OptErr Options -> OptErr Options)]
options =
  [ Option ['h', '?'] ["help"           ]
      (NoArg (onOpts $ \opts -> opts { optHelp    = True }))
      "show usage information"
  , Option ['V']      ["version"        ]
      (NoArg (onOpts $ \opts -> opts { optVersion = True }))
      "print the version information and exit"
  , Option ['d']      ["debug"          ]
      (NoArg (onOpts $ \opts -> opts { optDebug   = True }))
      "print resulting program to standard output"
  , Option []         ["assert"         ]
      (NoArg (onOpts $ \opts -> opts { optAssert  = True }))
      "check additional assertions during abstraction (slower)"
  , Option []         ["closed"         ]
      (NoArg (onOpts $ \opts -> opts { optClosed  = True }))
      "preserve closedness by not removing copy functions"
  , Option []         ["no-funpats"     ]
      (NoArg (onOpts $ \opts -> opts { optFunPats = False }))
      "do not automatically optimize functions defined using functional patterns"
  , option ['v']      ["verbosity"      ] verbDescriptions      "n"
      "verbosity level"
  , option []         ["color", "colour"] colorDescriptions     "mode"
      "coloring mode"
  , option ['S']      ["semantics"      ] semanticsDescriptions "semantics"
      "semantics for evaluation"
  , option ['A']      ["abstract"       ] abstractDescriptions  "mode"
      "abstraction mode"
  , option ['P']      ["proceed"        ] proceedDescriptions   "mode"
      "proceed mode"
  , Option []         ["suffix"         ]
      (ReqArg (\s -> onOpts $ \ opts -> opts { optSuffix = s }) "SUFFIX")
      "specify the suffix for the specialised program"
  ]

--- Verbosity descriptions
verbDescriptions :: OptTable Options
verbDescriptions = map toDescr verbosities
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set f opts = opts { optVerbosity = f }

--- Semantics descriptions
semanticsDescriptions :: OptTable Options
semanticsDescriptions = map toDescr semantics
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set s opts = opts { optSemantics = s }

--- Abstraction descriptions
abstractDescriptions :: OptTable Options
abstractDescriptions = map toDescr abstractions
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set a opts = opts { optAbstract = a }

--- ProceedMode descriptions
proceedDescriptions :: OptTable Options
proceedDescriptions = map toDescr proceedModes
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set p opts = opts { optProceedMode = p }

--- ColorMode descriptions
colorDescriptions :: OptTable Options
colorDescriptions = map toDescr colorModes
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set c opts = opts { optColorMode = c }

--- Retrieve the parsed options. This operation only returns if
---  * the @--help@ option was not specified
---  * the @--version@ option was not specified
---  * there were no errors in the specified options.
getOpts :: IO (Options, [String])
getOpts = do
  args <- getArgs
  prog <- getProgName
  parsed@(opts, _) <- processOpts prog $ parseOpts args
  when (optVerbosity opts >= Status) (putStrLn version)
  return parsed

--- Parse the options specified on the command line.
parseOpts :: [String] -> (Options, [String], [String])
parseOpts args = (opts, files, errs ++ argErrs)
  where
    (opts, argErrs)        = foldl (flip ($)) (defaultOptions, []) optErrs
    (optErrs, files, errs) = getOpt Permute options args

--- Process the parsed options.
processOpts :: String -> (Options, [String], [String])
            -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | optHelp    opts  = printUsage prog
  | optVersion opts  = printVersion
  | not (null errs') = badUsage prog errs'
  | otherwise        = do
    opts' <- setColoring opts
    return (opts', files)
  where errs' = errs ++ checkOpts opts files

--- Set the coloring mode when the specified mode is `CMAuto`.
setColoring :: Options -> IO Options
setColoring opts
  | optColorMode opts == CMAuto = do
    isTerminal <- hIsTerminalDevice stdout
    return opts { optColorMode = if isTerminal then CMAlways else CMNever }
  | otherwise   = return opts

--- Check the specified options for errors.
checkOpts :: Options -> [String] -> [String]
checkOpts _ []    = ["no files"]
checkOpts _ (_:_) = []

--- Print the usage information.
printUsage :: String -> IO a
printUsage prog = do
  putStrLn $ usageInfo header options
  exitWith 0
    where header = "usage: curry " ++ prog ++ " [OPTION] ... MODULE ..."

--- Complain about bad program usage (wrong options)
--- and print the usage information before exiting with error code 1.
badUsage :: String -> [String] -> IO a
badUsage prog errs = do
  mapIO_ (hPutStrLn stderr) errs
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitWith 1

--- Print the version string and exit.
printVersion :: IO a
printVersion = do
  putStrLn version
  exitWith 0
