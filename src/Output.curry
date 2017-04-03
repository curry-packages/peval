--- ----------------------------------------------------------------------------
--- This module defines some auxiliary functions for intermediate output.
---
--- @author  Björn Peemöller
--- @version September 2015
--- ----------------------------------------------------------------------------
module Output where

import IO        (hPutStrLn, stderr)
import List      (intercalate)
import System    (exitWith)
import Unsafe    (unsafePerformIO)

import PevalOpts (Options (..), ColorMode (..), Verbosity (..))

--- Trace details in pure computations.
traceDetail :: Options -> String -> a -> a
traceDetail o m x = unsafePerformIO (detail o m >> return x)

--- Trace debug information in pure computations.
traceDebug :: Options -> String -> a -> a
traceDebug o m x = unsafePerformIO (debug o m >> return x)

--- Print status information.
status :: Options -> String -> IO ()
status opts msg = when (optVerbosity opts >= Status) (putStrLn msg)

--- Print additional information.
info :: Options -> String -> IO ()
info opts msg = when (optVerbosity opts >= Info) (putStrLn msg)

--- Print detailed information.
detail :: Options -> String -> IO ()
detail opts msg = when (optVerbosity opts >= Detail) (putStrLn msg)

--- Print debug information.
debug :: Options -> String -> IO ()
debug opts msg = when (optVerbosity opts >= Debug) (putStrLn msg)

--- Assert the truth of the first value or throw an error with the second
--- argument as the information describing the failed assertion.
assert :: Bool -> String -> a -> a
assert b m x = if b then x
                    else unsafePerformIO (hPutStrLn stderr err >> exitWith 1)
  where err = "Assertion failed: " ++ m

--- Conditionally color the string, based on the given options.
colorWith :: Options -> (String -> String) -> String -> String
colorWith opts color s = if optColorMode opts == CMAlways then color s else s
