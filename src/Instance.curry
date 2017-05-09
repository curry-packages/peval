--- --------------------------------------------------------------------------
--- Computation of instance substitutions and generalizations.
---
--- @author  Björn Peemöller
--- @version April 2015
--- --------------------------------------------------------------------------
module Instance (instanceWith, instanceOf, msg) where

import List             (find, nub)
import Maybe            (isJust)
import Pretty           (pPrint, ($$), text)
import State            ( State, (<$>), (<*>), (>+), (>+=), concatMapS, getsS
                        , mapS, modifyS, returnS, runState)
import Utils            (disjoint, sameLength)

import FlatCurry.Types
import FlatCurryGoodies ( branchExprs, branchPats, samePattern, freeVars
                        , isConstrTerm, maxVar, patVars, maximumVarIndex)
import FlatCurryPretty  (ppExp, indent)
import Normalization    (eqRen, renameExprSubst)
import Output           (assert)
import Subst            ( Subst, ppSubst, emptySubst, combine, compose, dom
                        , restrict, singleSubst, subst, toSubst, varSubst
                        , isDetSubst )

-- ---------------------------------------------------------------------------
-- Instance computation
-- ---------------------------------------------------------------------------

--- Is the first expression a strict instance of the second expression?
strictInstanceOf :: Expr -> Expr -> Bool
strictInstanceOf e1 e2 = instanceOf e1 e2 && not (instanceOf e2 e1)

--- Is the first expression a deterministic instance of the second expression?
detInstanceOf :: Expr -> Expr -> Bool
detInstanceOf e1 e2 = case instanceWith e1 e2 of
  Nothing -> False
  Just s  -> isDetSubst e2 s

--- Is the first expression an instance of the second expression?
instanceOf :: Expr -> Expr -> Bool
instanceOf e1 e2 = isJust (instanceWith e1 e2)

--- `instanceWith e1 e2` tries to compute a substitution `sigma`
--- such that $e1 = \sigma(e2)$.
--- If `e1` is an instance of `e2`, the function
--- returns `Just sigma`, if not then `Nothing` is returned.
---
--- Note that `instance` requires the expressions to share the same structure,
--- i.e., no normalization is considered.
instanceWith :: Expr -> Expr -> Maybe Subst
instanceWith e1 e2 = case instance' re1 re2 of
  Nothing -> Nothing
  Just s  -> let s' = restrict (dom s2) $ compose s1 $ compose s s2
             in assert (subst s  re2 `eqRen` e1) "error in instance"
              $ Just s'
 where
  (re1, r1) = renameExprSubst e1
  (re2, r2) = renameExprSubst e2
  s1        = toSubst [(x, Var y) | (y, x) <- r1]
  s2        = toSubst [(x, Var y) | (x, y) <- r2]

--- We use the fact that variables free in the expression have a negative
--- variable index after `renameExprSubst`. Therefore, we can limit the
--- substitution to substitute variables with negative indizes only,
--- where the domain of the resulting substitution must not include locally
--- introduced variables.
instance' :: Expr -> Expr -> Maybe Subst
instance' ex1 ex2 = case (ex1, ex2) of
  (Var          x, Var          y)
    -- We create a substitution even for unbound variables because an unbound
    -- variable may not be bound to another expression later.
    | x == y                        -> Just $ if isUnboundVariable y
                                                 then singleSubst y ex1
                                                 else emptySubst
  (_             , Var          y)
    | all isUnboundVariable
        (y : freeVars ex1)          -> Just (singleSubst y ex1)
  (Lit          x, Lit          y)
    | x == y                        -> Just emptySubst
  (Comb c1 f1 es1, Comb c2 f2 es2)
    | c1 == c2 && f1 == f2
      && sameLength es1 es2         -> instanceL es1 es2
  (Let     ds1 e1, Let     ds2 e2)
    | sameLength ds1 ds2            -> let (vs1, bs1) = unzip ds1
                                           (vs2, bs2) = unzip ds2
                                       in  instanceL
                                           (map (varSubst vs1 vs2) (e1 : bs1))
                                           (e2 : bs2)
  (Free    vs1 e1, Free    vs2 e2)
    | sameLength vs1 vs2            -> instance' (varSubst vs1 vs2 e1) e2
  (Or       e1 f1, Or       e2 f2)  -> instanceL [e1,f1] [e2,f2]
  (Case c1 e1 b1, Case c2 e2 b2)
    | c1 == c2 && samePattern b1 b2 -> foldl union (instance' e1 e2)
                                       (zipWith instanceBranch b1 b2)
  (Typed   e1 ty1, Typed   e2 ty2)
    | ty1 == ty2                    -> instance' e1 e2
  _                                 -> Nothing

--- Is a variable unbound in the entire expression?
isUnboundVariable :: VarIndex -> Bool
isUnboundVariable v = v < 0

--- Instance computation for two lists of expressions.
instanceL :: [Expr] -> [Expr] -> Maybe Subst
instanceL es1 es2 = foldl union (Just emptySubst) (zipWith instance' es1 es2)

--- Instance for branch expression.
instanceBranch :: BranchExpr -> BranchExpr -> Maybe Subst
instanceBranch (Branch p1 e1) (Branch p2 e2)
  = instance' (varSubst (patVars p1) (patVars p2) e1) e2

--- Union of two substititutions with a check for a clashing substitution.
union :: Maybe Subst -> Maybe Subst -> Maybe Subst
union Nothing   _         = Nothing
union (Just _ ) Nothing   = Nothing
union (Just s1) (Just s2) = Subst.combine s1 s2

-- -----------------------------------------------------------------------------
-- Computation of the most specific generalizer (msg) of two expressions.
-- -----------------------------------------------------------------------------

--- `msg e e' = (g, sigma, theta)` returns the most specific generalization
--- `g` of `e` and `e'` and the substitutions `sigma` and `theta`,
--- such that $e = \sigma(g)$, and $e' = \theta(g)$.
--- A generalizer of two terms $e_1$, $e_2$ is a term $e$,
--- such that $\sigma(e) = e_1$ and $theta(e) = e_2$
--- ($e$ generalizes $e_1$ and $e_2$).
--- Furthermore, there is no other generalization $e'$ of $e_1$ and $e_2$,
--- such that $e' \neq e$ and $\tau(e) = e'$ ($e$ is most specific).
--- We do not rename the msg because otherwise the substitution may refer
--- to variables locally introduced in the msg.
msg :: Expr -> Expr -> (Expr, Subst, Subst)
msg e1 e2
  = let (g, state) = runState (msg' (e1, e2))
                              (initState (maximumVarIndex [e1, e2] + 1))
        l  = msgSubst state
        fv = freeVars g
        s1 = toSubst [ (v, e) | (v, e, _) <- l, v `elem` fv]
        s2 = toSubst [ (v, e) | (v, _, e) <- l, v `elem` fv]
    in  assertInstance e1 g s1 $ assertInstance e2 g s2 $ (g, s1, s2)
  where
  assertInstance e g s x = assert (eqRen (subst s g) e) (pPrint doc) x
    where doc = indent (text "Expression"        $$ ppExp   e)
             $$ indent (text "is no instance of" $$ ppExp   g)
             $$ indent (text "with substitution" $$ ppSubst s)

--- The Msg monad
type Msg a = State MsgState a

--- Internal state for msg computation
data MsgState = MsgState
  { msgFresh :: VarIndex
  , msgSubst :: [(VarIndex, Expr, Expr)]
  }

--- Initial state for msg computation
initState :: VarIndex -> MsgState
initState x = MsgState x []

--- Get a fresh variable
getFresh :: Msg VarIndex
getFresh = getsS   msgFresh                       >+= \i ->
           modifyS (\s -> s { msgFresh = i + 1 }) >+
           returnS i

--- Create a new substitution for two expressions
newSubst :: Expr -> Expr -> Msg Expr
newSubst e1 e2 = getsS msgSubst >+= \sub ->
                 getFresh >+= \j ->
                 modifyS (\s -> s { msgSubst = (j, e1, e2) : sub }) >+
                 returnS (Var j)

--- Get the substitution for two expressions
getSubst :: Expr -> Expr -> Msg Expr
getSubst e1 e2
  | all isConstrTerm [e1, e2]
  = getsS msgSubst >+= \sub ->
    case find (\(_, s1, s2) -> e1 == s1 && e2 == s2) sub of
      Just (v, _, _) -> returnS (Var v)
      Nothing        -> newSubst e1 e2
  | otherwise = newSubst e1 e2

--- `substsUseLocalVars vs es` checks whether one of the substitutions for the
--- variables in `es` substitutes any of the variables in `vs`.
--- This is used to guarantee that the substitution does not affect locally
--- introduced variables.
substsUseLocalVars :: [VarIndex] -> [Expr] -> Msg Bool
substsUseLocalVars vs es =
  concatMapS substsFor (nub $ concatMap freeVars es) >+= \es' ->
  returnS $ not $ disjoint vs (nub $ concatMap freeVars es')
 where
  substsFor v = getsS msgSubst >+= \sub ->
                case find (\(x, _, _) -> x == v) sub of
                  Just (_ , e1, e2) -> returnS [e1, e2]
                  Nothing           -> returnS []

--- Compute the most specific generalization of two expressions.
msg' :: (Expr, Expr) -> Msg Expr
msg' p@(ex1, ex2) = case p of
  (Var         x, Var         y) | x == y
    ->  returnS ex1
  (Lit         x, Lit         y) | x == y
    ->  returnS ex1
  (Comb c1 f1 e1, Comb c2 f2 e2) | c1 == c2 && f1 == f2 && sameLength e1 e2
    ->  Comb c1 f1 <$> mapS msg' (zip e1 e2)
  (Let    ds1 e1, Let    ds2 e2) | sameLength ds1 ds2
    ->  mapS msgVar (zip vs1 vs2)                     >+= \vs   ->
        let es1' = map (varSubst vs1 vs) es1
            es2' = map (varSubst vs2 vs) es2 in
        mapS msg' (zip es1' es2')                     >+= \es'  ->
        msg' (varSubst vs1 vs e1, varSubst vs2 vs e2) >+= \e'   ->
        substsUseLocalVars vs (e':es')                >+= \used ->
        if used then getSubst ex1 ex2 else returnS (Let (zip vs es') e')
    where (vs1, es1) = unzip ds1
          (vs2, es2) = unzip ds2
  (Free    xs e1, Free    ys e2) | sameLength xs ys
    ->  mapS msgVar (zip xs ys)                     >+= \zs   ->
        msg' (varSubst xs zs e1, varSubst ys zs e2) >+= \e'   ->
        substsUseLocalVars zs [e']                  >+= \used ->
        if used then getSubst ex1 ex2 else returnS (Free zs e')
  (Or      x1 y1, Or      x2 y2)
    ->  Or <$> msg' (x1, x2) <*> msg' (y1, y2)
  (Case c1 e1 b1, Case c2 e2 b2) | c1 == c2 && samePattern b1 b2
    ->  msg' (e1, e2)              >+= \e  ->
        mapS msgBranch (zip b1 b2) >+= \bs ->
        let vs = nub $ concatMap patVars (branchPats bs) in
        substsUseLocalVars vs (branchExprs bs) >+= \used ->
        if used then getSubst ex1 ex2 else returnS $ Case c1 e bs
  (Typed  x1 ty1, Typed  x2 ty2)
    | ty1 == ty2                 -> flip Typed ty1 <$> msg' (x1, x2)
  (_            , _            ) -> getSubst ex1 ex2

--- Compute the most specific generalization of two branch expressions.
msgBranch :: (BranchExpr, BranchExpr) -> Msg BranchExpr
msgBranch (Branch p1 e1, Branch p2 e2) = case (p1, p2) of
  (LPattern l  , LPattern   _) -> Branch (LPattern l) <$> msg' (e1, e2)
  (Pattern c xs, Pattern _ ys) -> mapS msgVar (zip xs ys) >+= \zs ->
                                  Branch (Pattern c zs) <$>
                                  let e1' = varSubst xs zs e1
                                      e2' = varSubst ys zs e2
                                  in  msg' (e1', e2')
  _                            -> error "Instance.msgBranch"

--- Compute the most specific generalization
--- of two locally introduced variables.
msgVar :: (VarIndex, VarIndex) -> Msg VarIndex
msgVar (x, y) = if x == y then returnS x else getFresh
