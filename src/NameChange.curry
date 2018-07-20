--- --------------------------------------------------------------------------
--- Change the names of functions in an entire program.
---
--- @author  Björn Peemöller
--- @version September 2015
--- --------------------------------------------------------------------------
module NameChange (NameChange, ncRenaming, ncResultants, ncFunc, ncExpr) where

import Data.Tuple.Extra  (second)
import Data.Maybe        (fromMaybe)

import FlatCurry.Types
import PevalBase (FunLhs, Renaming, Resultant)

type NameChange = [(QName, QName)]

--- Change the names in a renaming.
ncResultants :: NameChange -> [Resultant] -> [Resultant]
ncResultants nc res = [ (ncFunLhs nc lhs, ncExpr nc e) | (lhs, e) <- res ]

--- Change the names in a renaming.
ncRenaming :: NameChange -> Renaming -> Renaming
ncRenaming nc ren = [ (ncExpr nc e, ncFunLhs nc lhs) | (e, lhs) <- ren ]

--- Change the names in a function's left-hand side.
ncFunLhs :: NameChange -> FunLhs -> FunLhs
ncFunLhs nc (f, vs) = (ncQName nc f, vs)

--- Change the names in a function declaration.
ncFunc :: NameChange -> FuncDecl -> FuncDecl
ncFunc nc (Func qn a v ty r) = Func (ncQName nc qn) a v ty (ncRule nc r)

--- Change the names in a function rule.
ncRule :: NameChange -> Rule -> Rule
ncRule nc   (Rule  vs e) = Rule vs (ncExpr nc e)
ncRule _  e@(External _) = e

--- Change the names in an expression.
ncExpr :: NameChange -> Expr -> Expr
ncExpr _  v@(Var       _) = v
ncExpr _  l@(Lit       _) = l
ncExpr nc (Comb ct qn es) = Comb ct (ncQName nc qn) (map (ncExpr nc) es)
ncExpr nc (Let      bs e) = Let     (map (second (ncExpr nc)) bs) (ncExpr nc e)
ncExpr nc (Free     vs e) = Free vs (ncExpr nc e)
ncExpr nc (Or      e1 e2) = Or      (ncExpr nc e1) (ncExpr nc e2)
ncExpr nc (Case  ct e bs) = Case ct (ncExpr nc e) (map (ncBranch nc) bs)
ncExpr nc (Typed    e ty) = Typed   (ncExpr nc e) ty

--- Change the names in a case branch.
ncBranch :: NameChange -> BranchExpr -> BranchExpr
ncBranch nc (Branch p e) = Branch p (ncExpr nc e)

--- Change the name of a single function.
ncQName :: NameChange -> QName -> QName
ncQName nc qn = fromMaybe qn (lookup qn nc)
