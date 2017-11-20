--- ----------------------------------------------------------------------------
--- This module performs the abstraction of expressions.
--- Depending on the criteria used for abstractions, this module comes to a
--- decision if parts of the given expressions should be further evaluated.
--- This part is crucial to ensure termination of the whole process.
---
--- *Note:* In contrast to the original work we do not remove expressions
--- from the set during abstraction because we want to avoid both
--- re-evaluation of expressions or the loss of the more specific results.
---
--- @author  Elvira Albert, German Vidal, Michael Hanus, Björn Peemöller
--- @version September 2015
--- ----------------------------------------------------------------------------
module Abstract (abstract) where

import AnsiCodes        (yellow)
import Function         (on, second)
import List             (delete, find, maximumBy, sum)
import Maybe            (isJust)
import Text.Pretty      (Doc, (<+>), ($$), (<>), equals, pPrint, text, vsep)
import Utils            (none, sameLength)

import FlatCurry.Types
import FlatCurryGoodies ( branchExprs, completePartCall, isVar, isConsCall
                        , onBranchExps, prelApply, prelude, samePattern, sq
                        , sq', subExprs, isFailed, funcName, getSQ)
import FlatCurryPretty  (ppExp, indent)
import Instance         (instanceWith, instanceOf, msg)
import Normalization    (eqNorm, normalizeExpr)
import Output           (assert, colorWith, debug, traceDetail)
import PevalOpts        (Options (optAbstract, optAssert), Abstraction (..))
import Subst            (ppSubst, rng)

--- The sequence of expressions to be further evaluated.
--- We use a sequence instead of a set to be able to use the anti-transitivity
--- of the well-founded ordering.
type AbsSet = [Expr]

--- Abstraction operator.
--- @param otps - Options passed to partiall evaluator.
--- @param p    - program.
--- @param q    - already abstracted expressions.
--- @param es   - new expressions to be abstracted.
abstract :: Options -> Prog -> AbsSet -> [Expr] -> AbsSet
abstract opts (Prog _ _ _ fs _) q es
  | optAssert opts = originProp $ closednessProp $ orderingProp q'
  | otherwise      = q'
 where
  q' = absAll opts q es
  qes = q ++ es
  -- origin property: abstraction does not "invent" new expressions
  originProp     q0 = assert (originateFrom q0 qes)
                      ("Abstraction: origin property violated\n"
                      ++ pPrint (vsep (map ppExp q0))
                      ++ "\ndoes not originate from\n"
                      ++ pPrint (vsep (map ppExp qes)))
                      q0
  -- closedness property: abstraction subsumes all expressions
  closednessProp q0 = assert (null notClosed)
                      ("Abstraction: closedness property violated\n"
                      ++ pPrint (vsep (map ppExp notClosed))
                      ++ "\nis/are not closed with respect to\n"
                      ++ pPrint (vsep (map ppExp q0)))
                      q0
    where notClosed = filter (not . closed (map funcName fs) q0) qes
  -- ordering property: abstraction fulfills the property of the used ordering
  orderingProp   q0 = case optAbstract opts of
    None -> q0
    WFO  -> assert (decreasing   q0)
            "Abstraction: decreasing size property violated" q0
    WQO  -> assert (nonembedding q0)
            "Abstraction: nonembedding property violated" q0

--- Trace an abstraction action.
traceAbs :: Options -> Doc -> a -> a
traceAbs opts doc x
  = traceDetail opts (colorWith opts yellow str) x
  where str = pPrint (text "Abstraction:" <+> doc) ++ "\n"

--- Trace that the addition of an expression to the set is absorbed
--- because it is already contained in the set.
traceContained :: Options -> AbsSet -> Expr -> AbsSet
traceContained opts q e = traceAbs opts doc q
  where doc = indent (text "Set contains expression" $$ ppExp e)

--- Add a new expression to the set (useful for tracing).
--- The expression is renamed before it is added so that the invariant
--- that all expressions in the set are renamed is asserted.
addNew :: Options -> AbsSet -> Expr -> AbsSet
addNew opts q e = traceAbs opts doc (e' : q)
  where doc = indent (text "Adding expression" $$ ppExp e')
        e'  = normalizeExpr e

--- Is an expression already contained in the set of expressions, either as
--- a renaming or a constructor instance (modulo normalization)?
contained :: Options -> AbsSet -> Expr -> Bool
contained _ q e = normalizeExpr e `elem` q

--- Abstraction of a list of expressions.
absAll :: Options -> AbsSet -> [Expr] -> AbsSet
absAll opts q = foldl (abs opts) q

--- abstraction for terms not within square brackets.
--- See [Albert et al. 1998, p. 803, definition 5.11]
abs :: Options -> AbsSet -> Expr -> AbsSet
abs _    q (Var          _) = q -- ignored
abs _    q (Lit          _) = q -- ignored
abs opts q c@(Comb  _ _ es) = case getSQ c of
  Just e -> absRedex opts q (complete e)
  _      -> absAll   opts q es
abs opts q (Let       bs e) = absAll   opts q (e : map snd bs)
abs opts q (Free       _ e) = abs      opts q e
abs opts q (Or       e1 e2) = absAll   opts q [e1, e2]
abs opts q (Case    _ e bs) = absAll   opts q (e : branchExprs bs)
abs opts q (Typed      e _) = abs      opts q e

--- Abstract a reducible expression, i.e., a call to a user-defined function
--- or an expression surrounded by square brackets.
--- The behaviour is parametric w.r.t. the abstraction mode.
absRedex :: Options -> AbsSet -> Expr -> AbsSet
absRedex opts q e
  | isVar e            = q
  | contained opts q e = if not (evaluable e) then abs opts q (sq' e)
                                              else traceContained opts q e
  | otherwise          = case optAbstract opts of
    None -> addNew opts q e
    WFO  -> absWfo opts q e
    WQO  -> absWqo opts q e

--- Compute whether an expression is evaluable by the partial evaluation
--- semantics at all. In short, evaluable are:
---  * variables which are bound in the surrounding context
---  * function calls
---  * case expressions not applied to an unbound variable
---  * let-bindings where the subjacent expression is evaluable
---  * Non-determinism where any of the subjacent expressions is evaluable
evaluable :: Expr -> Bool
evaluable e0 = eval [] e0
  where
  eval vs (Var         x) = x `elem` vs
  eval _  (Lit         _) = False
  eval vs c@(Comb ct _ _) = case getSQ c of
    Just e -> eval vs e
    _      -> ct == FuncCall -- || any (not . isVar) es
  eval vs (Let      ds e) = eval (vs ++ map fst ds) e
  eval vs (Free     xs e) = eval (vs ++ xs) e
  eval vs (Or      e1 e2) = eval vs e1 || eval vs e2 -- True?
  eval _  (Case    _ _ _) = True
  eval vs (Typed     e _) = eval vs e

--- Complete a partial function call to a regular function call
complete :: Expr -> Expr
complete e = case e of
  Comb ct@(FuncPartCall _) qn es -> completePartCall ct qn es
  _                              -> e

-- -----------------------------------------------------------------------------
-- Abstraction based on a well-founded ordering (size of expression)
-- -----------------------------------------------------------------------------

--- Abstraction based on a well-founded order.
absWfo :: Options -> [Expr] -> Expr -> [Expr]
absWfo opts q e = case firstComparable e q of
  Nothing                    -> addNew opts q e
  Just  c | size e <= size c -> addNew opts q e
          | otherwise        -> absMsg opts q e c

--- Find the best comparable expression in a list of expressions
--- w.r.t the given expression. A comparable must share the same root expression
--- and is the first one found, i.e., the smallest one.
firstComparable :: Expr -> [Expr] -> Maybe Expr
firstComparable e es = case filter (`comparable` e) es of
  []  -> Nothing
  c:_ -> Just c

--- Decreasing size property of the sequence.
decreasing :: AbsSet -> Bool
decreasing []     = True
decreasing (e:es) = all (\e' -> size e' >= size e) (filter (`comparable` e) es)
                 && decreasing es

--- Size of an expression, inducing a well-founded ordering.
--- A variable has a size of zero because we want expression like `1` or `True`
--- to be of a greater size since they contain more information.
--- Integer literals are translated into constructor terms for termination.
size :: Expr -> Int
size (Var         _) = 0
size (Lit         l) = case l of
  Charc  _ -> 1
  Floatc _ -> 1
  Intc   i -> size $ int2Expr i
size c@(Comb _ _ es) = case getSQ c of
  Just e -> size e
  _      -> 1 + sum (map size es)
size (Let      bs e) = 1 + sum (map size (e : map snd bs))
size (Free      _ e) = 1 + size e
size (Or      e1 e2) = 1 + size e1 + size e2
size (Case   _ e bs) = 1 + sum (map size (e : branchExprs bs))
size (Typed     e _) = size e

-- -----------------------------------------------------------------------------
-- Abstraction based on a well-quasi ordering (homeomorphic embedding)
-- -----------------------------------------------------------------------------

--- abstraction based on a well-quasi order.
absWqo :: Options -> [Expr] -> Expr -> [Expr]
absWqo opts q e
  | contained opts q e = traceContained opts q e
  | otherwise          = case embeddedPre e q of
    Nothing -> addNew opts q e
    Just e' -> absMsg opts q e e'

--- Nonembedding property of the sequence
nonembedding :: AbsSet -> Bool
nonembedding []     = True
nonembedding (e:es) = none (\e' -> embedded e' e) (filter (`comparable` e) es)
                   && nonembedding es

--- Well-quasi order based on the embedding.
--- Integer literals are translated into constructor terms for termination.
embeddedPre :: Expr -> AbsSet -> Maybe Expr
embeddedPre e es = find (\e' -> comparable e' e && embedded e' e) es

--- Is the first expression embedded in the second expression?
embedded :: Expr -> Expr -> Bool
embedded e1 e2 = embedded' e1 e2 || embeddedArg e1 e2

--- Is the first expression directly embedded in the second expression, i.e.,
--- the outermost symbols coincide and the arguments are (not necessarily
--- directly) embedded.
embedded' :: Expr -> Expr -> Bool
embedded' ex1 ex2 = case (ex1, ex2) of
  (Var           _, Var           _) -> True
  (Lit  (Charc  x), Lit  (Charc  y)) -> x == y
  (Lit  (Floatc x), Lit  (Floatc y)) -> x == y
  (Lit  (Intc   x), Lit  (Intc   y)) -> embedded (int2Expr x) (int2Expr y)
  (Comb  c1 f1 es1, Comb  c2 f2 es2) -> c1 == c2 && f1 == f2
                                        && allEmbedded es1 es2
  (Let      ds1 e1, Let      ds2 e2) -> length ds1 <= length ds2
                                        && embedded e1 e2
                                        && allEmbedded' (map snd ds1)
                                                        (map snd ds2)
  (Free      xs e1, Free      ys e2) -> length xs <= length ys
                                        && embedded e1 e2
  (Or        e1 f1, Or        e2 f2) -> embedded e1 e2 && embedded f1 f2
  (Case  c1 e1 bs1, Case  c2 e2 bs2) -> c1 == c2 && samePattern bs1 bs2
                                        && allEmbedded (e1 : branchExprs bs1)
                                                       (e2 : branchExprs bs2)
  (Typed    e1 ty1, Typed    e2 ty2) -> ty1 == ty2  && embedded e1 e2
  _                                  -> False

--- Are the expressions in the first list embedded in the expression from
--- the second list, assuming an equal list length?
allEmbedded :: [Expr] -> [Expr] -> Bool
allEmbedded xs ys = and (zipWith embedded xs ys)

--- Are the expressions in the first list embedded in the expression from
--- the second list, assuming that the first list may be shorter
--- than the second list?
allEmbedded' :: [Expr] -> [Expr] -> Bool
allEmbedded' []     _        = True
allEmbedded' (_:_)  []       = False
allEmbedded' (e:es) (e':es') = (embedded e e' && allEmbedded' es es')
                            || allEmbedded' (e:es) es'

-- --- Is the first expression strictly embedded in the second expression?
-- strictEmbedded :: Expr -> Expr -> Bool
-- strictEmbedded e1 e2 = embedded e1 e2 && not (e1 `strictInstanceOf` e2)

--- Is the first expression embedded in some sub-expression
--- of the second expression?
embeddedArg :: Expr -> Expr -> Bool
embeddedArg _ (Var        _) = False
embeddedArg _ (Lit        _) = False
embeddedArg x (Comb  _ _ es) = any (embedded x) es
embeddedArg x (Let     bs e) = any (embedded x) (e : map snd bs)
embeddedArg x (Free     _ e) = x `embedded` e
embeddedArg x (Or     e1 e2) = any (embedded x) [e1, e2]
embeddedArg x (Case  _ e bs) = any (embedded x) (e : branchExprs bs)
embeddedArg x (Typed    e _) = x `embedded` e

-- -----------------------------------------------------------------------------
-- Auxiliary functions for abstraction
-- -----------------------------------------------------------------------------

--- Abstract two expression by computing their most specific generalization.
absMsg :: Options -> AbsSet -> Expr -> Expr -> AbsSet
absMsg opts q new old = traceAbs opts doc res
  where
  res
    -- There is no specific generalization, thus we add the components of e.
    -- Because of case-expressions, free variables and let-bindings, it is
    -- possible that two expressions can not be generalized, for instance
    -- consider the example `listUni`.
    | isVar g      = traceAbs opts (text "Ignoring variable msg" <+> ppExp g)
                   $ abs opts q (sq' new)
    -- The expression to add equals the generalization.
    -- We remove the matching comparable expression to avoid
    -- a non-terminating loop, and add the generalization
    -- as well as the range of the substitutions.
    | eqNorm g new = absAll opts (delete old q) (map sq $ g : rng s1 ++ rng s2)
    -- We add both the generalization and the range of the substitution.
    | otherwise    = absAll opts q (map sq $ g : rng s1 ++ rng s2)

  (g, s1, s2) = msg new old

  doc = vsep $ map indent
    [ text "Generalizing new expression"     $$ ppExp new
    , text "and old expression"              $$ ppExp old
    , text "to most-specific generalization" $$ ppExp g
    , text "with substitutions" $$ text "sigma" <+> equals <+> ppSubst s1
                                $$ text "theta" <+> equals <+> ppSubst s2
    ]

--- Two expressions are comparable if their root symbols coincide.
comparable :: Expr -> Expr -> Bool
comparable ex1 ex2 = case (ex1, ex2) of
  (Var             _, Var             _) -> True
  (Lit             _, Lit             _) -> True
  (Comb FuncCall a [f,_], Comb FuncCall b [g,_])
    -- We don't want to generalize `apply`, therefore we ignore it here.
    | a == prelApply && b == prelApply   -> comparable f g
  (Comb      c1 f1 _, Comb      c2 f2 _) -> c1 == c2 && f1 == f2
  (Free          _ _, Free          _ _) -> True
  (Or            _ _, Or            _ _) -> True
  (Case     c1 _ bs1, Case     c2 _ bs2) -> c1 == c2 && samePattern bs1 bs2
  (Let           _ _, Let           _ _) -> True
  (Typed       _ ty1, Typed       _ ty2) -> ty1 == ty2
  _                                      -> False

--- Translates an integer literal to a constructor term
--- to be able to apply ordering.
int2Expr :: Int -> Expr
int2Expr x | x <  0    = Comb ConsCall (prelude "-") [int2Expr (-x)]
           | x < 10    = digit x
           | otherwise = Comb ConsCall (prelude ":") [digit d, int2Expr m]
  where
    digit n = Comb ConsCall (prelude $ show n) []
    (d, m)  = x `divMod` 10

-- -----------------------------------------------------------------------------
-- closedness test (assertion)
-- -----------------------------------------------------------------------------

--- Does every expression in the abstraction set originate from some
--- expression in the expression list?list of expressions
originateFrom :: AbsSet -> [Expr] -> Bool
originateFrom q es = all (\e -> any (\e' -> complete e' `instanceOf` e) es') q
  where es' = [ e' | e <- es, e' <- subExprs e, not (isVar e') ]

--- `allClosed p q es` computes whether all expressions in `es` are closed
--- with respect to the sequence `q`, where `p` contains the user-defined
--- functions, i.e, functions not contained in `p` are considered primitive.
allClosed :: [QName] -> AbsSet -> [Expr] -> Bool
allClosed p q es = all (closed p q) es

--- `allClosed p q e` computes whether the expression `e` is closed
--- with respect to the sequence `q`, where `p` contains the user-defined
--- functions, i.e, functions not contained in `p` are considered primitive.
closed :: [QName] -> AbsSet -> Expr -> Bool
closed p q e = case e of
  Var _             -> True
  Lit _             -> True
  Comb ct qn es
    | isConsCall ct -> allClosed p q es
    | otherwise     -> case getSQ e of
      Just e'       -> closed p q e' || recClosed q e'
      _             -> isFailed e || recClosed q e ||
                       (isPrimitive p qn && allClosed p q es)
  Let ds e'         -> allClosed p q (e' : map snd ds) || recClosed q e
  Free _ e'         -> closed p q e' || recClosed q e
  Or e1 e2          -> (closed p q e1 && closed p q e2) || recClosed q e
  Case _ e' bs      -> allClosed p q (e' : branchExprs bs) || recClosed q e
  Typed e' _        -> recClosed q e || recClosed q e'
 where
  recClosed []      _  = False
  recClosed (q':qs) e' = case instanceWith (complete e') q' of
    Just s  -> allClosed p q (rng s) || recClosed qs e'
    Nothing -> recClosed qs e'

--- Is a function considered primitive, i.e., not contained in the given
--- list of user-defined functions?
isPrimitive :: [QName] -> QName -> Bool
isPrimitive fs qn = qn `notElem` fs
