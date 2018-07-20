--- ----------------------------------------------------------------------------
--- This module implements normalization of FlatCurry programs and expressions.
--- This includes
---   *  $\alpha$-conversion (renaming of variables)
---   * flattening (lifting nested applications to let bindings)
---   * Ordering of let declarations in the order in which the bound
---     variables occur in the expression.
---
--- Furthermore, a compression of expressions is implemented which simplifies
--- expressions by
---   * Removing unused variable bindings
---   * Removing failing non-deterministic branches
---   * Substituting variable bindings which are not shared
---   * Removing failing alternatives in case expression
---
--- @author  Björn Peemöller
--- @version April 2015
--- ----------------------------------------------------------------------------
module Normalization
  ( -- Normalization
    eqNorm, eqRen, normalizeExpr, normalizeFreeExpr, simplifyExpr
    -- Generation of fresh variants
  , freshResultant, freshRule
    -- Renaming
  , renameResultant, renameFuncDecl, renameFreeExpr, renameExpr, renameExprSubst
  ) where

import Data.Tuple.Extra (second)
import Data.List        (intersect, mapAccumL, partition)

import FlatCurry.Types
import FlatCurryGoodies ( addPartCallArg, eqPattern, failedExpr, freeVars
                        , freeVarsDup, maxVar, isLit, topSQ, getSQ
                        , func, isConsCall, isPartCall, isVar, maxVarIndex
                        , maximumVarIndex, isSQ, isFailed, findBranch
                        , mkCase, mkFree, mkLet, mkOr, patVars, sq
                        , prelApply, prelCond, prelCond', trExpr)
import Output           (assert)
import Utils            (count, sameLength)
import PevalBase        (Resultant)
-- import SCC              (scc)
import Subst            (mkSubst, singleSubst, subst, varSubst)

-- -----------------------------------------------------------------------------
-- Normalization
-- -----------------------------------------------------------------------------

eqNorm :: Expr -> Expr -> Bool
eqNorm x y = normalizeExpr x == normalizeExpr y

--- Normalization of an expression which may contain free variables.
--- This includes:
--- 1) Compression of the expression (see below).
--- 1) The order of let-bound variables and free variables is changed
---    to the order of the occurences of the bindings in the expression.
--- 1) Unbound variables are renumbered from -1 downwards, bound variables
---    from 1 upwards in the order of their occurence.
normalizeExpr :: Expr -> Expr
normalizeExpr = renameExpr . orderDecls

--- Like `normalizeExpr`, but unbound variables in the expression
--- retain their indizes.
normalizeFreeExpr :: Expr -> Expr
normalizeFreeExpr = renameFreeExpr . orderDecls

--- Renaming of an expression such that afterwards the variables defined in the
--- expression are numbered from max(1, maxFreeVar + 1) upwards
--- and the variables free in the expression retain their indizes.
renameFreeExpr :: Expr -> Expr
renameFreeExpr e = freshExpr (maxVar (freeVars e) + 1) e

--- Renaming of an expression such that the variables defined in the
--- expression are numbered from max(1, maxFreeVar + 1) upwards
--- and the variables free in the  expression from -1 downwards.
--- The resulting substitution contains the mapping from the former unbound
--- variables to the latter unbound variables.
renameExprSubst :: Expr -> (Expr, [(VarIndex, VarIndex)])
renameExprSubst e
  = assert (uncurry (flip varSubst) (unzip sub) e' `eqRen` e) "renameExprSubst"
    (e', sub)
 where
  e'  = snd $ rnExpr sub (newVars $ maxVar fvs + 1) e
  fvs = freeVars e
  sub = zip fvs [-1, -2 ..]

-- -----------------------------------------------------------------------------
-- Fresh variants and renaming
-- -----------------------------------------------------------------------------

--- Create a fresh variant of a resultant by numbering all variables
--- from `i` onwards.
freshResultant :: VarIndex -> Resultant -> Resultant
freshResultant i ((f, vs), e) = ((f, vs'), e')
  where (Rule vs' e') = snd $ rnRule (newVars i) (Rule vs e)

--- Create a fresh variant of a pattern by numbering all variables
--- from `i` onwards.
freshPattern :: VarIndex -> Pattern -> Pattern
freshPattern i p = snd (rnPattern (newVars i) p)

--- Create a fresh variant of a rule by numbering all variables
--- from `i` onwards.
freshRule :: VarIndex -> Rule -> Rule
freshRule i = snd . rnRule (newVars i)

--- Create a fresh variant of an expression by numbering all local variables
--- from `i` onwards.
freshExpr :: VarIndex -> Expr -> Expr
freshExpr i = snd . rnExpr [] (newVars i)

--- Renaming of a resultant such that the variables are numbered
--- from 1 onwards, starting with the function's parameters.
renameResultant :: Resultant -> Resultant
renameResultant = freshResultant 1

--- Renaming of a function declaration such that the variables are numbered
--- from 1 onwards, starting with the function's parameters.
renameFuncDecl :: FuncDecl -> FuncDecl
renameFuncDecl = snd . rnFunc (newVars 1)

--- Are two expression equal up to a renaming of local variables?
eqRen :: Expr -> Expr -> Bool
eqRen x y = renameExpr x == renameExpr y

--- Renaming of an expression such that afterwards the variables defined in the
--- expression are numbered from 1 upwards and the variables free in the
--- expression are numbered from -1 downwards.
renameExpr :: Expr -> Expr
renameExpr e = snd $ rnExpr (zip (freeVars e) [-1, -2 ..]) (newVars 1) e

-- -----------------------------------------------------------------------------
-- Generation of variables
-- -----------------------------------------------------------------------------

--- Create an infinite list of new variables.
newVars :: VarIndex -> [VarIndex]
newVars x = [ x .. ]

takeVars :: [VarIndex] -> [a] -> ([VarIndex], [VarIndex])
takeVars fresh  []     = (fresh, [])
takeVars (f:fs) (_:os) = let (fs', os') = takeVars fs os in (fs', f : os')
takeVars []     (_:_)  = error "Renaming.takeVars: no more fresh variables"

-- -----------------------------------------------------------------------------
-- Implementation of renaming
-- -----------------------------------------------------------------------------

type Renaming a = [VarIndex] -> a -> ([VarIndex], a)

--- Renaming of a 'Prog', i.e., all variables occurring in the 'Prog'
--- get renamed to @v1@, @v2@, ... consistently ($\alpha$-conversion).
rnProg :: Renaming Prog
rnProg xs (Prog m is ty fs os) = (xs1, Prog m is ty fs' os)
  where (xs1, fs') = mapAccumL rnFunc xs fs

--- Renaming of a function declaration.
rnFunc :: Renaming FuncDecl
rnFunc xs (Func f a v ty r) = second (Func f a v ty) (rnRule xs r)

--- Renaming of a function rule.
rnRule :: Renaming Rule
rnRule xs (Rule  vs e) = let (xs1, vs') = takeVars xs vs
                             (xs2, e' ) = rnExpr (zip vs vs') xs1 e
                         in  (xs2, Rule vs' e')
rnRule xs (External s) = (xs, External s)

--- Renaming of an expression.
rnExpr :: [(VarIndex, VarIndex)] -> Renaming Expr
rnExpr ren xs (Var x) = case lookup x ren of
  Nothing -> (xs, Var x)
  Just  w -> (xs, Var w)
rnExpr _   xs l@(Lit     _) = (xs, l)
rnExpr ren xs (Comb ct qn es)
  = let (xs1, es') = mapAccumL (rnExpr ren) xs es
    in  (xs1, Comb ct qn es')
rnExpr ren xs (Free vs e)
  = let (xs1, vs') = takeVars xs vs
        ren1       = zip vs vs' ++ filter ((`notElem` vs) . fst) ren
        (xs2, e' ) = rnExpr ren1 xs1 e
    in  (xs2, Free vs' e')
rnExpr ren xs (Let ds e)
  = let (vs , es ) = unzip ds
        (xs1, vs') = takeVars xs ds
        ren1       = zip vs vs' ++ filter ((`notElem` vs) . fst) ren
        (xs2, es') = mapAccumL (rnExpr ren1) xs1 es
        (xs3, e' ) = rnExpr ren1 xs2 e
    in  (xs3, Let (zip vs' es') e')
rnExpr ren xs (Or e1 e2)
  = let (xs1, e1') = rnExpr ren xs  e1
        (xs2, e2') = rnExpr ren xs1 e2
    in  (xs2, Or e1' e2')
rnExpr ren xs (Case ct e bs)
  = let (xs1, e' ) = rnExpr ren xs e
        (xs2, bs') = mapAccumL (rnBranchExpr ren) xs1 bs
    in  (xs2, Case ct e' bs')
rnExpr ren xs (Typed e ty)
  = let (xs1, e') = rnExpr ren xs e
    in  (xs1, Typed e' ty)

rnBranchExpr :: [(VarIndex, VarIndex)] -> Renaming BranchExpr
rnBranchExpr ren ys (Branch (Pattern p zs) be)
  = let (ys1, zs') = takeVars ys zs
        ren1       = zip zs zs' ++ filter ((`notElem` zs) . fst) ren
        (ys2, be') = rnExpr ren1 ys1 be
    in  (ys2, Branch (Pattern p zs') be')
rnBranchExpr ren ys (Branch l@(LPattern _) be)
  = let (ys1, be') = rnExpr ren ys be
    in  (ys1, Branch l be')

rnPattern :: Renaming Pattern
rnPattern xs (Pattern p ys) = second (Pattern p) (takeVars xs ys)
rnPattern xs l@(LPattern _) = (xs, l)

-- -----------------------------------------------------------------------------
-- Compression of an expression
-- -----------------------------------------------------------------------------

--- Order the bindings of locally introduced variables in the order
--- of their occurence in the expression. If any variable is unused,
--- it is removed.
orderDecls :: Expr -> Expr
orderDecls = trExpr Var Lit Comb oFree Or Case Branch oLet Typed
  where
  oFree vs e = let vs' = filter (`elem` vs) (freeVars e)
               in  assert (sameLength vs vs') "orderDecls"
               mkFree vs' e
--              $ foldr (\v e' -> mkFree [v] e') e vs'
  oLet  ds e = mkLet  (extract ds (freeVars e)) e
--              foldr mkLet e (scc (\(v, _) -> [v]) (\(_, b) -> freeVars b) ds)

  extract []       _      = []
  extract (_:_)    []     = error "Normalization.orderDecls"
  extract ds@(_:_) (v:vs) = case break ((== v) . fst) ds of
    (_  , []         ) -> extract ds vs
    (ds1, (_, e'):ds2) -> (v, e') : extract (ds1 ++ ds2) (vs ++ freeVars e')

--- Simplification of FlatCurry expressions. This consists of the following steps:
--- * Locally introduces free variables that are not used are removed.
--- * `Or` applied to `failed` is reduced to the alternative expression.
--- * If `Or` is applied to two calls to `cond` sharing the same first
---   condition, the condition is lifted above the `Or` construct.
--- * If `Or` is applied to two `case` expressions scrutinizing the same
---   expression, the `case` expression is lifted upwards and the corresponding
---   branches are combined using `Or`.
--- * Case branches directly calling `failed` are removed. If all branches
---   are removed, the expression is reduced to `failed` instead.
--- * Evaluation annotations `SQ e` are moved downwards until a potentially
---   evaluable expression in encountered.
--- * Let-bindings which refer to another variable or are only used once
---   are inserted and removed afterwards.
--- * Unused let-bindings are removed.
simplifyExpr :: Expr -> Expr
simplifyExpr = trExpr Var Lit cComb cFree cOr mkCase Branch cLet Typed
  where
  -- compression of annotations, see FlatCurryGoodies for sq
  cComb ct f es = let call = Comb ct f es in case getSQ call of
                                              Just e -> sq e
                                              _      -> call
  -- compression of free variables: remove unused variables
  cFree vs e = mkFree (vs `intersect` freeVars e) e
  -- compression of non-determinism
  cOr e1 e2 = case (e1, e2) of
    (Comb FuncCall f [a, b], Comb FuncCall g [c, d])
      | all (`elem` [prelCond, prelCond']) [f, g]
        && a == c                -> simplifyExpr $ func prelCond [a, Or b d]
    (Case ct1 e1' bs1, Case ct2 e2' bs2)
      | ct1 == ct2 && e1' == e2' -> simplifyExpr $ Case ct1 e1'
                                                   (mergeBranches bs1 bs2)
    _ | isSQ e1 || isSQ e2       -> simplifyExpr (topSQ (Or e1 e2))
      | otherwise                -> mkOr e1 e2

  mergeBranches []                     bs2 = bs2
  mergeBranches (b@(Branch p1 e1):bs1) bs2
    = case break (\(Branch p' _) -> eqPattern p1 p') bs2 of
        (_   , []                 ) -> b : mergeBranches bs1 bs2
        (bs21, Branch p2 e2 : bs22) ->
          let p' = freshPattern (maximumVarIndex [e1, e2]) p1
              e' = Or (varSubst (patVars p1) (patVars p') e1)
                      (varSubst (patVars p2) (patVars p') e2)
          in  Branch p' (simplifyExpr e') : mergeBranches bs1 (bs21 ++ bs22)

  -- compression of let-declarations
  cLet ds e
    | ds == ds' && e == e' = mkLet ds' e'
    | otherwise            = cLet  ds' e'
    where
    (ds', e') = cLet' [] ds
    cLet' bs0 []        = (bs0, e)
    cLet' bs1 (b : bs2)
      | isInlineable b  = (map (second replace) (bs1 ++ bs2), replace e)
      | otherwise       = cLet' (bs1 ++ [b]) bs2
      where
      isInlineable bd  = uncurry isSimple bd || not (isShared bd)
      isShared (v, _ ) = count v (concatMap freeVarsDup (e : map snd ds)) > 1
      replace          = simplifyExpr . subst (uncurry singleSubst b)

    isSimple v ve = case ve of
      Var        x -> x /= v -- do not replace recursive bindings
                             -- such as let ones = 1 : ones in ones
      Lit        _ -> True
      Comb ct _ es -> (ct == ConsCall || isPartCall ct) && all (isSimple v) es
      _            -> isFailed ve

  -- Compression of case expressions: When the scrutinized expression is either
  -- a literal or a constructor call, the respective branch is searched for.
  -- If such a branch exists, the expressions reduces to the branch's
  -- right-hand-side, otherwise the expression reduces to `failed`.
  cCase ct e bs = case e of
    Lit l              -> case findBranch (LPattern l) bs of
      Nothing       -> failedExpr
      Just ( _, be) -> be
    Comb ConsCall c es -> case findBranch (Pattern c []) bs of
      Nothing       -> failedExpr
      Just (xs, be) -> simplifyExpr (unfold xs es be)
    _                  -> mkCase ct e bs

--- Simple unfolding
unfold :: [VarIndex] -> [Expr] -> Expr -> Expr
unfold vs es e = mkLet (zip vs' es) e'
  where Rule vs' e' = freshRule (maximumVarIndex es + 1) (Rule vs e)

-- -----------------------------------------------------------------------------
-- Flattening
-- -----------------------------------------------------------------------------

--- A flattening takes an additional list of fresh variables
type Flattening a = [VarIndex] -> a -> ([VarIndex], a)

--- Flatten a program
flattenProg :: Flattening Prog
flattenProg xs (Prog m is ty fs os) = (xs1, Prog m is ty fs' os)
  where (xs1, fs') = mapAccumL flattenFunc xs fs

--- Flatten a function
flattenFunc :: Flattening FuncDecl
flattenFunc xs (Func f a v ty r) = second (Func f a v ty) (flattenRule xs r)

--- Flatten a rule
flattenRule :: Flattening Rule
flattenRule xs (Rule  vs e) = second (Rule vs) (flattenExpr xs e)
flattenRule xs (External s) = (xs, External s)

--- Flatten an expression
flattenExpr :: Flattening Expr
flattenExpr xs v@(Var         _) = (xs, v)
flattenExpr xs l@(Lit         _) = (xs, l)
flattenExpr xs (Comb   ct qn es) = let (xs1, ds, es') = splitArgs xs es
                                   in  (xs1, flatLet ds (Comb ct qn es'))
flattenExpr xs (Free       vs e) = let (xs1, e' ) = flattenExpr xs e
                                   in  (xs1, Free vs e')
flattenExpr xs (Let        ds e) = let (xs1, ds') = mapAccumL flatD xs ds
                                       (xs2, e' ) = flattenExpr xs1 e
                                   in  (xs2, flatLet ds' e')
    where flatD ys (v, ve)       = let (ys1, ve') = flattenExpr ys ve
                                   in  (ys1, (v, ve'))
flattenExpr xs (Or        e1 e2) = let (xs1, e1') = flattenExpr xs  e1
                                       (xs2, e2') = flattenExpr xs1 e2
                                   in  (xs2, Or e1' e2')
flattenExpr xs (Case    ct e bs) = let (xs1, e' ) = flattenExpr xs e
                                       (xs2, bs') = mapAccumL flatB xs1 bs
                                   in  (xs2, Case ct e' bs')
    where flatB ys (Branch p be) = let (ys1, be') = flattenExpr ys be
                                   in  (ys1, Branch p be')
flattenExpr xs (Typed      e ty) = let (xs1, e' ) = flattenExpr xs e
                                   in  (xs1, Typed e' ty)

--- @splitArgs xs es = (xs', ds, es')@ replaces all non-variable
--- expressions in @es@ by new variables subsequently taken from @xs@,
--- and generates the bindings @ds@ for those lifted expressions.
--- That is, @es'@ consists of variables only, originating either from @es@
--- or from @xs@, such that $es'[ds] = es$, i.e., replacing the extracted
--- bindings again should yield the original list.
--- @xs'@ is xs without the extracted variables.
splitArgs :: [VarIndex] -> [Expr] -> ([VarIndex], [(VarIndex, Expr)], [Expr])
splitArgs xs []     = (xs, [], [])
splitArgs xs (e:es) = case e of
  Var _      -> let (xs', ds,     es') = splitArgs xs es
                in  (xs', ds, e : es')
  _          -> let (x' : xs', e') = flattenExpr xs e
                    (xs2, ds, es') = splitArgs xs' es
                in  (xs2, (x', e') : ds, Var x' : es')

--- @flatLet ds e@ lifts nested let-declarations in ds to the top-level.
--- In addition, if `e` has the form `Let ds' e'`, the bindings `ds'` are
--- also lifted upwards.
flatLet :: [(VarIndex, Expr)] -> Expr -> Expr
flatLet decls ex = case liftDecls decls of
  []  -> ex
  ds' -> case ex of
    Let ds'' e' -> Let (ds' ++ ds'') e'
    _           -> Let ds' ex
 where
  liftDecls [] = []
  liftDecls ((x, d) : ds) = case d of
    Let ds1 e -> ds1 ++ (x, e) : liftDecls ds
    _         ->        (x, d) : liftDecls ds
