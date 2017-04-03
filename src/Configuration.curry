--- --------------------------------------------------------------------------
--- This module defines some configuration options for the partial evaluator.
--- It is to shared by the partial evaluator itself and the test driver
--- to allow some reuse of option parsing.
---
--- @author  Björn Peemöller
--- @version April 2015
--- --------------------------------------------------------------------------
module Configuration where

--- Mode to control coloring of output.
--- @cons CMAlways - Always color the output.
--- @cons CMAuto   - Only color when stdout is connected to a terminal
--- @cons CMNever  - Never color the output.
data ColorMode = CMAlways | CMAuto | CMNever

--- Description and flag of coloring modes
colorModes :: [(ColorMode, String, String)]
colorModes =
  [ (CMAlways, "always", "Always color the output"                          )
  , (CMAuto  , "auto"  , "Only color when stdout is connected to a terminal")
  , (CMNever , "never" , "Never color the output"                           )
  ]

--- Semantics used for unfolding.
--- @cons RLNT         - see paper xxx
--- @cons Natural      - see paper yyy
--- @cons LetRewriting - see paper zzz
data Semantics = RLNT | Natural | LetRW

--- Description and flag of optimization levels
semantics :: [(Semantics, String, String)]
semantics =
  [ (RLNT   , "rlnt"   , "RLNT semantics"   )
  , (Natural, "natural", "Natural semantics")
  , (LetRW  , "letrw"  , "Let rewriting"    )
  ]

--- Kind of abstraction operator.
--- @cons None - no generalization (termination is not ensured).
--- @cons WFO  - generalization based on a well-founded order
---              (termination ensured).
--- @cons WQO  - generalization based on a well-quasi order.
---              Termination is ensured even for integers by
---              translating them to expressions.
--- Note that only None and WQO really pass the "KMP-test".
data Abstraction = None | WFO | WQO

--- Description and flag of abstractions
abstractions :: [(Abstraction, String, String)]
abstractions =
  [ (None, "none", "no generalization (termination is not ensured)")
  , (WFO , "wfo" , "generalization based on a well-founded order"  )
  , (WQO , "wqo" , "generalization based on a well-quasi order"    )
  ]

--- Mode to control function unfolding.
--- @cons One  - Unfold only one function call at all
--- @cons Each - Unfold only one call for each function
--- @cons All  - Unfold all function calls, may not terminate!
data ProceedMode = PMNone | PMOne | PMEach | PMAll

--- Description and flag of optimization levels
proceedModes :: [(ProceedMode, String, String)]
proceedModes =
  [ (PMOne , "one" , "Perform only one unfolding"               )
  , (PMEach, "each", "Perform one unfolding for each function"  )
  , (PMAll , "all" , "Perform all unfoldings, may not terminate")
  ]
