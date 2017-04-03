--- ----------------------------------------------------------------------------
--- This module defines the recursive application $ren_\rho$
--- of a renaming $\rho$ to an expression.
--- This is related to substitutions, but substitutes expressions surrounded
--- by angle brackets by other expressions instead of substituting variables.
--- Note that function calls are not directly renamed since they are expected
--- to be annotated by angle brackets by the unfolding operation.
---
--- @version September 2015
--- ----------------------------------------------------------------------------
module Renaming (renameExpr) where

import AnsiCodes        (yellow)
import Function         (second)
import List             (find)

import Pretty           (($$), (<+>), pPrint, text)
import Utils            (dropLast)

import FlatCurry.Types
import FlatCurryGoodies (completePartCall, onBranchExps, sq', getSQ)
import FlatCurryPretty  (ppExp, indent)
import Instance         (instance)
import Normalization    (normalizeFreeExpr)
import Output           (colorWith, traceDetail)
import PevalBase        (Renaming, ppRenaming, mkFuncCall)
import PevalOpts        (Options (optAbstract), Abstraction (..))
import Subst            (Subst, isDetSubst, isVariableRenaming, subst)

--- Rename an expression according to the independent renaming rho.
--- The renaming only takes place for (partial) function calls and expressions
--- surrounded by square brackets. It should mimic the behaviour of the current
--- abstraction operator.
renameExpr :: Options -> Renaming -> Expr -> Expr
renameExpr opts rho e = rename (initEnv opts rho) e

--- The renaming environment.
data RenameEnv = RenameEnv
  { rnOptions   :: Options
  , rnRenaming  :: Renaming
  }

--- Create the initial renaming environment.
initEnv :: Options -> Renaming -> RenameEnv
initEnv opts rho = RenameEnv opts rho

--- Trace a renaming action.
rnTrace :: RenameEnv -> Expr -> Expr -> Expr
rnTrace RenameEnv { rnOptions = o } e e'
  = traceDetail o (colorWith o yellow str) e'
  where str = pPrint (indent (text "Renaming" $$ ppExp e )
                   $$ indent (text "to"       $$ ppExp e')) ++ "\n"

--- Rename an expression by replacing sub-expressions according to the given
--- renaming. This is mainly a traversal of the structure, except for the
--- interesting cases of expressions annotated by `SQ`.
rename :: RenameEnv -> Expr -> Expr
rename _   v@(Var         _) = v
rename _   l@(Lit         _) = l
rename env c@(Comb ct qn es) = case getSQ c of
  Just e -> renameSQ env e
  _      -> Comb ct qn (map (rename env) es)
rename env (Free       vs e) = Free vs    (rename env e)
rename env (Case    ct e bs) = Case ct    (rename env e)
                                          (rename env `onBranchExps` bs)
rename env (Or        e1 e2) = Or         (rename env e1) (rename env e2)
rename env (Let        bs e) = Let        (map (second (rename env)) bs)
                                          (rename env e)
rename env (Typed      e ty) = Typed      (rename env e) ty

--- Rename an annotated expression, defaults to a (recursive) renaming of the
--- the expression if there is no renaming for the entire expression.
--- For a partial call, we complete it to a function using fresh variables and
--- later remove them again. Because the new call may require less arguments
--- than the old one, we can not keep the old number of arguments but must
--- remove the number of added variables, so that the result will again be
--- a partial function call.
--- For instance, the partial call `const 1` will be extended to `const 1 v1`,
--- and may be replaced by `pe0 v1` where `pe0 v1 = 1`. The result must then
--- be `pe0` but not `pe0 1`. This is safe because the new function will at
--- least contain all those variables as free variables that we just have added.
renameSQ :: RenameEnv -> Expr -> Expr
renameSQ env e = case e of
  Comb ct@(FuncPartCall n) f es -> let e' = completePartCall ct f es
                                       Comb FuncCall f' es' = renameRedex env e'
                                   in  Comb ct f' (dropLast n es')
  _                             -> renameRedex env e

--- Rename an expression based on the abstraction behaviour. In case of no
--- abstraction, the expression must be a variant e of another
--- expression, otherwise it must be an deterministic instance,
--- whereas variants are preferred to obtain a better specialization.
renameRedex :: RenameEnv -> Expr -> Expr
renameRedex env e
  | pea == None = renameVariant env e def
  | otherwise   = renameVariant env e (renameInstance env e def)
  where
  def = rename env (sq' (normalizeFreeExpr e))
  pea = optAbstract (rnOptions env)

--- Rename an expression to a call to a specialized function
--- when it is a variant of another expression that was evaluated.
--- Because the substitution is a variable renaming, the call will only be
--- applied to variables, hence no further renaming is necessary.
--- If an expression is no variant of any expression that was evaluated,
--- then the default value is returned.
renameVariant :: RenameEnv -> Expr -> Expr -> Expr
renameVariant env e def = case findInstance env (\_ -> isVariableRenaming) e of
  Nothing -> def
  Just e' -> rnTrace env e e'

--- Rename an expression when it is a deterministic instance of another
--- expression or return the default value otherwise.
--- Because the result will always be a function call to a resultant function,
--- we call `sq'` to rename the function's arguments.
renameInstance :: RenameEnv -> Expr -> Expr -> Expr
renameInstance env e def = case findInstance env isDetSubst e of
  Nothing -> def
  Just e' -> rename env (sq' (rnTrace env e e'))

--- Find an instance for an expression that satisfies the given predicate.
--- If such an instance is found, the expression is rewritten to the instance.
findInstance :: RenameEnv -> (Expr -> Subst -> Bool) -> Expr -> Maybe Expr
findInstance env p e = lookupInstance (rnRenaming env)
  where
  lookupInstance []               = Nothing
  lookupInstance ((e', lhs) : es) = case instance (normalizeFreeExpr e) e' of
    Just s | p e' s -> Just (subst s (mkFuncCall lhs))
    _               -> lookupInstance es
