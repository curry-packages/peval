--- --------------------------------------------------------------------------
--- Substitutions on FlatCurry expressions.
---
--- @author  Björn Peemöller
--- @version April 2015
--- --------------------------------------------------------------------------
module Subst
  ( Subst, ppSubst, emptySubst, singleSubst, mkSubst, toSubst, dom, rng
  , restrict, without, compose, combine, lookupSubst, findSubstWithDefault
  , fmapSubst, substSingle, subst, varSubst, isVariableRenaming, isDetSubst
  ) where

import List             (nub)
import Pretty           (Doc, (<+>), ($$), rarrow, pPrint, semiBracesSpaced, text)
import Utils            (disjoint)

import FlatCurry.Types
import FlatCurryGoodies (isConstrTerm, freeVarsDup, isVar, patVars)
import FlatCurryPretty  (ppVarIndex, ppExp, indent)
import Output           (assert)
import Utils            (count)

--- Data type for substitutions
data Subst = Subst [(VarIndex, Expr)]

--- Pretty printing of a substitution
ppSubst :: Subst -> Doc
ppSubst (Subst []     ) = text "{}"
ppSubst (Subst s@(_:_)) = semiBracesSpaced (map ppVarBinding s)
  where ppVarBinding (v, e) = ppVarIndex v <+> rarrow <+> ppExp e

--- empty substitution
emptySubst :: Subst
emptySubst = Subst []

--- substitute a variable by an expression
singleSubst :: VarIndex -> Expr -> Subst
singleSubst v e = Subst [(v, e)]

--- create a substitution from a list of variables and a list of expressions
mkSubst :: [VarIndex] -> [Expr] -> Subst
mkSubst vs es = toSubst (zip vs es)

--- create a substitution from a list of variable/expression pairs.
toSubst :: [(VarIndex, Expr)] -> Subst
toSubst = Subst

--- extract domain of a given substitution
dom :: Subst -> [VarIndex]
dom (Subst s) = map fst s

--- extract range of a given substitution
rng :: Subst -> [Expr]
rng (Subst s) = map snd s

--- Restrict a substititution to a given domain.
restrict :: [VarIndex] -> Subst -> Subst
restrict vs (Subst s) = Subst [ s' | s'@(v, _) <- s, v `elem` vs ]

--- Remove a given part of the domain from a substititution.
without :: [VarIndex] -> Subst -> Subst
without vs (Subst s) = Subst [ s' | s'@(v, _) <- s, v `notElem` vs ]

--- `compose s1 s2` composes the substitutions s1 and s2.
--- The result is `s1 . s2`, i.e., it has the same effect as `s1(s2(t))`
--- when applied to a term `t`.
compose :: Subst -> Subst -> Subst
compose s1 s2 =
  let Subst t1 = fmapSubst (subst s1) s2
      Subst t2 = without   (dom   s2) s1
  in  Subst (t1 ++ t2)

--- Try to combine two substitutions $\sigma$ and $\theta$.
--- This operation returns `Nothing` if there exists a conflicting substitution,
--- i.e., there exists a variable $x \in \dom(\sigma)$, $x \in \dom(\theta)$
--- such that $\sigma(x) \neq $\theta(x)$.
combine :: Subst -> Subst -> Maybe Subst
combine s1@(Subst ve1) s2@(Subst ve2)
  | clash s1 s2 = Nothing
  | otherwise   = Just $ toSubst $ nub $ ve1 ++ ve2

--- Do two substituions clash, i.e., do they define a different mapping
--- for at least one variable?
clash :: Subst -> Subst -> Bool
clash (Subst s1) (Subst s2)
  = not $ null [ () | (v1, e1) <- s1, (v2, e2) <- s2, v1 == v2 && e1 /= e2 ]

--- Lookup a substititution for a variable.
--- If there is no substitution given, the variable is substituted by itself.
lookupSubst :: VarIndex -> Subst -> Expr
lookupSubst v s = findSubstWithDefault (Var v) v s

--- Find a substititution for a variable with a given default value.
findSubstWithDefault :: Expr -> VarIndex -> Subst -> Expr
findSubstWithDefault def v (Subst s) = case lookup v s of
  Nothing -> def
  Just e  -> e

--- `fmap f sigma` applies `f` on all expressions in the range of sigma.
--- That is, if $\sigma = \{ v_1 \mapsto e_1 \dots v_n \mapsto e_n \}$, then
--- `fmap f sigma` equals \{ v_1 \mapsto f(e_1) \dots v_n \mapsto f(e_n) \}$
fmapSubst :: (Expr -> Expr) -> Subst -> Subst
fmapSubst f (Subst s) = Subst [ (v, f e) | (v, e) <- s ]

--- `substSingle v s e` substitutes `v` by `s` in `e`
substSingle :: VarIndex -> Expr -> Expr -> Expr
substSingle v s e = subst (singleSubst v s) e

--- Replace a list of variables by another list of variables
--- in the given expression.
varSubst :: [VarIndex] -> [VarIndex] -> Expr -> Expr
varSubst xs ys e = subst (mkSubst xs (map Var ys)) e

--- Is the substititution `sigma` a variable renaming, i.e., are the expressions
--- in `rng(sigma)` all variables?
isVariableRenaming :: Subst -> Bool
isVariableRenaming = all isVar . rng

--- Is the substititution `sigma` a deterministic substitution w.r.t `e`, i.e.,
--- are the expressions in `rng(sigma)` all either constructor terms or does
--- the variable occur at most once in e?
isDetSubst :: Expr -> Subst -> Bool
isDetSubst e (Subst s) = all isDet s
  where isDet (v, e') = isConstrTerm e' || count v (freeVarsDup e) <= 1

--- `subst sigma e = sigma(e)` substitutes all occurrences of variables
---  by corresponding expressions.
---  substitute all occurrences of $v_i$ by $e_i$ in $e$
---  if $s = \{ v_1 \mapsto e_1 \dots v_n \mapsto e_n \}$
---
--- Note: The substititution must not replace variables introduced in the
--- expression itself, either as a free variable, a let binding or a pattern
--- variable. This is guaranteed and checked as an assertion.
--- If the assertion is violated, an error is thrown.
subst :: Subst -> Expr -> Expr
subst s v@(Var         x) = findSubstWithDefault v x s
subst _ l@(Lit         _) = l
subst s (Comb    ct c es) = Comb ct c (map (subst s) es)
subst s f@(Free     vs e) = assert (disjoint vs (dom s))
                                   (errSubst "free variable" s f)
                          $ Free vs   (subst s e)
subst s (Or        e1 e2) = Or        (subst s e1) (subst s e2)
subst s c@(Case  ct e cs) = Case ct   (subst s e) (map substBran cs)
  where
  substBran (Branch p be) = assert (disjoint (patVars p) (dom s))
                                   (errSubst "pattern variable" s c)
                          $ Branch p (subst s be)
subst s l@(Let      bs e) = assert (disjoint (map fst bs) (dom s))
                                   (errSubst "let variable" s l)
                          $ Let (map substBinding bs) (subst s e)
  where substBinding (v, ve) = (v, subst s ve)
subst s (Typed     e ty) = Typed     (subst s e) ty

--- Error message thrown when a locally bound variable appears in the domain
--- of the substitution.
errSubst :: String -> Subst -> Expr -> String
errSubst what s e = pPrint $
     text "Sustitution for"              <+> text what
  $$ text "Could not apply substitution" <+> indent (ppSubst s)
  $$ text "to expression "               <+> indent (ppExp e)
