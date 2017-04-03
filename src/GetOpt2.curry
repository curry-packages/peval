--- --------------------------------------------------------------------------
--- Extension of the `GetOpt` command line argument parser.
--- This module defines some utility functions to allow the parsing of option
--- arguments to also handle errors in the arguments.
---
--- @author  Björn Peemöller
--- @version September 2015
--- --------------------------------------------------------------------------
module GetOpt2 (module GetOpt, module GetOpt2) where

import GetOpt
import List   (intercalate, maximum)
import Utils  (rpad)

--- Type synonym for option and error message
type OptErr opts = (opts, [String])

--- An option table is a list of triples consisting of the option string,
--- its description and the effect on the global option set.
type OptTable opts = [(String, String, opts -> opts)]

--- Lift a function on options to options and errors.
onOpts :: (opts -> opts) -> OptErr opts -> OptErr opts
onOpts f (opts, errs) = (f opts, errs)

--- Lift a function on a string and options to options and errors.
onOptsArg :: (String -> opts -> opts) -> String -> OptErr opts -> OptErr opts
onOptsArg f arg (opts, errs) = (f arg opts, errs)

--- Add an error message to a option/errors pair.
addErr :: String -> OptErr opts -> OptErr opts
addErr err (opts, errs) = (opts, errs ++ [err])

--- Convert an option table to option descriptions capable of error handling.
option :: String -> [String] -> OptTable opts -> String -> String
       -> OptDescr (OptErr opts -> OptErr opts)
option flags longFlags tbl arg what = Option flags longFlags
  (ReqArg (parseOptErr what tbl) arg)
  ("set " ++ what ++ " `" ++ arg ++ "', where `" ++ arg ++ "' is one of\n"
    ++ renderOptErrTable tbl)

--- Parsing function for option specified as an option table.
parseOptErr :: String -> OptTable opts -> String -> OptErr opts -> OptErr opts
parseOptErr what table opt = case lookup3 opt table of
  Just f  -> onOpts f
  Nothing -> addErr $ "unrecognized " ++ what ++ '`' : opt ++ "'\n"
 where
  lookup3 _ []                  = Nothing
  lookup3 k ((k', _, v2) : kvs)
    | k == k'                   = Just v2
    | otherwise                 = lookup3 k kvs

--- Rendering of an option specified using an option table.
renderOptErrTable :: OptTable opts -> String
renderOptErrTable ds = intercalate "\n"
                     $ map (\(k, d, _) -> "  " ++ rpad maxLen k ++ ": " ++ d) ds
  where maxLen = maximum $ map (\(k, _, _) -> length k) ds
