--- ----------------------------------------------------------------------------
--- This module defines some basic types and function related to partial
--- evaluation.
---
--- @version April 2015
--- ----------------------------------------------------------------------------
module PevalBase
  ( FunLhs, mkFuncCall
  , Resultant, ppResultants, ppResultant
  , Renaming, ppRenaming
  ) where

import Pretty hiding (indent)

import FlatCurry.Types
import FlatCurryGoodies
import FlatCurryPretty

--- The left-hand-side of a function consists of its name
--- and a list of variables.
type FunLhs = (QName, [VarIndex])

--- Pretty printing of a function's left-hand side.
ppLhs :: FunLhs -> Doc
ppLhs = ppExp . mkFuncCall

--- Create a function call from a function's left-hand-side.
mkFuncCall :: FunLhs -> Expr
mkFuncCall (f, vs) = Comb FuncCall f (map Var vs)

--- A resultant represents a new function definition consisting of a left-hand
--- side and an expression (the right-hand side).
type Resultant = (FunLhs, Expr)

--- Pretty printing of a list of resultants.
ppResultants :: [Resultant] -> Doc
ppResultants = compose (<$+$>) . map ppResultant

--- Pretty printing of a resultant.
ppResultant :: Resultant -> Doc
ppResultant (lhs, rhs) = hsep [ppLhs lhs, equals, ppExp rhs]

--- A renaming associates expressions with a function call.
type Renaming = [(Expr, FunLhs)]

--- Pretty printing of a renaming.
ppRenaming :: Renaming -> Doc
ppRenaming = compose (<$+$>) . map ppRen
  where ppRen (l, r) = indent (ppExp l) $$ char '\x21d2' <+> indent (ppLhs r)
