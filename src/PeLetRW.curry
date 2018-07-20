--- --------------------------------------------------------------------------
--- This module performs the actual partial evaluation of a given expression,
--- based on let-rewriting [PPDP'07].
--- The problem with the let-rewriting calculus is that it does not
---   * consider recursive let-expressions
---   * provide an operational semantics
---
--- @author  Björn Peemöller
--- @version April 2015
--- --------------------------------------------------------------------------
module PeLetRW (pevalExpr) where

import Data.Tuple.Extra (second)
import Data.List        (find, intersect, maximum)
import Data.Maybe       (fromJust)
import Control.Monad.Trans.State

import Text.Pretty      (pPrint)
import FlatCurry.Types
import FlatCurryGoodies
import FlatCurryPretty  (ppExp)
import Normalization    (freshRule)
import Output           (traceDetail)
import PevalOpts        (Options)
import Subst            (mkSubst, subst, singleSubst, varSubst)

pevalExpr :: Options -> Prog -> Expr -> Expr
pevalExpr opts (Prog _ _ _ fs _) e
  = evalState (peval e) (PEState opts fs (maxVarIndex e) 1)

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

--- Internal state of partial evaluation, containing
---   * the list of all defined function declarations,
---     used for unfolding (read-only).
---   * Renaming index for fresh variables.
---   * Number of already performed unfolding operations,
---     used for local termination.
data PEvalState = PEState Options [FuncDecl] Int Int

type PEM a = State PEvalState a

getOpts :: PEM Options
getOpts = fmap (\(PEState o _ _ _) -> o) get

lookupRule :: QName -> PEM (Maybe Rule)
lookupRule f
  = do PEState _ fs _ _ <- get
       let mbFunc = find (hasName f) fs
       return $ case mbFunc of
         Nothing              -> Nothing
         Just (Func _ _ _ _r) -> Just r

incrRenamingIndex :: Int -> PEM Int
incrRenamingIndex j
  = get >>= \(PEState o fs i d) -> put (PEState o fs (i + j) d) >> return i

incrDepth :: PEM ()
incrDepth = modify $ \(PEState o fs i d) -> PEState o fs i (d + 1)

-- local criteria if we can proceed unfolding a call in a given state
proceed :: PEM Bool
proceed = fmap (\(PEState _ _ _ d) -> d < 1) get

orElse :: PEM a -> PEM a -> PEM a
orElse act alt = proceed >>= \should ->
  if should then incrDepth >> act else alt

traceM :: String -> PEM a -> PEM a
traceM msg x = getOpts >>= \opts -> traceDetail opts msg x

-- ---------------------------------------------------------------------------
-- Partial evaluation
-- ---------------------------------------------------------------------------

--- peval implements the non-standard meta-interpreter.
--- @param e   - expression to partially evaluate
--- @return      partially evaluated expression
peval :: Expr -> PEM Expr
peval x = traceM ("pe: " ++ pPrint (ppExp x)) (peval' x)
  where
  peval' v@(Var        _) = return v      -- (VAR)
  peval' l@(Lit        _) = return l      -- (LIT)
  peval' c@(Comb ct f es) = case getSQ c of
    Just e -> peval e        -- (SQ)
    _      -> peComb ct f es -- (COMB)
  peval' (Let       ds e) = peLet  ds e
  peval' (Free      vs e) = peFree vs e
  peval' (Or       e1 e2) = peOr   e1 e2
  peval' (Case   ct e bs) = peCase ct e bs
  peval' (Typed     e ty) = flip Typed ty <$> peval e -- (TYPED)

--- partial evaluation of combination
peComb :: CombType -> QName -> [Expr] -> PEM Expr
peComb ct f es = splitArgs es >>= \(ds', es') ->
  if null ds'
    then case ct of
      FuncCall -> peFuncCall f es                 -- (FUNC)
      _        -> Comb ct f <$> mapM peval es -- (CONS, PARTC, PARTF)
    else peval (Let ds' (Comb ct f es'))

splitArgs :: [Expr] -> PEM ([(VarIndex, Expr)], [Expr])
splitArgs []     = return ([], [])
splitArgs (e:es)
  | isVar e   = splitArgs es >>= \(ds', es') -> return (ds', e : es')
  | otherwise = incrRenamingIndex 1 >>= \newVar ->
                splitArgs es >>= \(ds', es') ->
                return ((newVar, e) : ds', Var newVar : es')

--- partial evaluation of function application (Fapp).
peFuncCall :: QName -> [Expr] -> PEM Expr
peFuncCall f es
  = lookupRule f >>= \mbRule -> case mbRule of
      Nothing -> peBuiltin f es
      Just r  -> (unfold r es >>= peval)
                 `orElse` return (topSQ (Comb FuncCall f es))

--- unfolding of right-hand-side.
unfold :: Rule -> [Expr] -> PEM Expr
unfold r@(Rule _ e) es
  = incrRenamingIndex (maxVarIndex e) >>= \renIndex ->
    let Rule vs' e' = freshRule renIndex r
    in  return (subst (mkSubst vs' es) e')
unfold (External _) _ = error "PeLetRW.unfold: external"

--- partial evaluation of let expression.
peLet :: [(VarIndex, Expr)] -> Expr -> PEM Expr
peLet ds e = case e of
    -- (LET-VAR)
    Var v               -> case lookup v ds of
                             Nothing -> return e
                             Just  b -> peval (Let ds b)
                                        `orElse` return (topSQ (Let ds e))
    -- (LET-LIT)
    Lit _               -> return e
    -- (LET-CONS)
    Comb ConsCall c es  -> peval $ Comb ConsCall c (map (Let ds) es)
    Comb FuncCall _ _
      -- (LET-FAILED)
      | e == failedExpr -> return failedExpr
      -- (LET-EVAL-1)
      | otherwise        -> case getSQ e of
        -- (LET-SQ)
        Just e' -> peval (Let ds e')
        _       -> letEval
      -- (LET-PART)
    Comb partcall q es   -> peval $ Comb partcall q (map (Let ds) es)
    -- (LET-EVAL-2)
    Let ds' e'           -> peval $ Let (ds ++ ds') e'
    -- (LET-FREE)
    Free vs e'           ->
      incrRenamingIndex (maxVar vs) >>= \renIndex ->
      let vs' = map (+ renIndex) vs
      in  peval $ Free vs' (Let ds (varSubst vs vs' e'))
    -- (LET-OR)
    Or e1 e2             -> peval $ Or (Let ds e1) (Let ds e2)
    -- (LET-CASE)
    Case ct e' bs        -> peCase ct e' bs -- peval (Case ct (Let ds e') (Let ds `onBranchExps` bs))
    -- (LET-TYPED)
    Typed e' ty          -> peval (Typed (Let ds e') ty)
    -- (CASE-FUNPAT)
  where letEval = peval e >>= \e' ->
                  let let' = Let ds e' in
                  if e == e' then return let' else peval let'

--- partial evaluation of free variables (FREE).
--- If we could make some progress in evaluating the subject expression,
--- we strip unused free variables and proceed with partial evaluation.
peFree :: [VarIndex] -> Expr -> PEM Expr
peFree vs e = peval e >>= \e' ->
              let vs'   = vs `intersect` freeVars e'
                  free' = if null vs' then e' else Free vs' e' in
              if e' /= e then peval free' else return free'

--- Partial evaluation of non-deterministic choice (OR).
peOr ::  Expr -> Expr -> PEM Expr
peOr e1 e2
  | e1 == failedExpr = peval e2
  | e2 == failedExpr = peval e1
  | otherwise = peval e1 >>= \e1' ->
                if e1' /= e1
                  then peval (Or e1' e2)
                  else peval e2 >>= \e2' ->
                    if e2' /= e2
                    then peval (Or e1 e2')
                    else return (Or e1 e2)

--- Partial evaluation of case expressions.
peCase :: CaseType -> Expr -> [BranchExpr] -> PEM Expr
peCase ct subj bs = case subj of
  -- (CASE-VAR)
  Var v                  -> Case ct subj <$> mapM peBranch bs
    where peBranch (Branch p be) = Branch p <$>
                                   peval (subst (singleSubst v (pat2exp p)) be)
  -- (CASE-LIT)
  Lit l                  -> return $ matchLit bs
    where
    matchLit []                            = failedExpr
    matchLit (Branch (LPattern p) e : bes)
      | p == l    = e
      | otherwise = matchLit bes
    matchLit (Branch (Pattern _ _) _ : _)
      = error "PartEval.peCase.matchLit: Constructor pattern"
  -- (CASE-CONS)
  Comb ConsCall c es     -> peval (matchCons bs)
    where
    matchCons []                              = failedExpr
    matchCons (Branch (Pattern p vs) e : bes)
      | p == c    = subst (mkSubst vs es) e
      | otherwise = matchCons bes
    matchCons (Branch (LPattern _) _ : _)
      = error "PartEval.peCase.matchCons: Literal pattern"
  Comb FuncCall _ _
      -- (CASE-FAILED)
    | subj == failedExpr     -> return failedExpr
      -- (CASE-EVAL-1)
    | otherwise              -> case getSQ subj of
      -- (CASE-SQ)
      Just e -> peval (Case ct e bs)
      _      -> caseEval
  -- (CASE-ERROR)
  Comb (ConsPartCall _ ) _ _ -> error "PeRLNT.peCase: ConsPartCall"
  Comb (FuncPartCall _ ) _ _ -> error "PeRLNT.peCase: FuncPartCall"
  -- (CASE-EVAL-2)
  Let _ _                    -> caseEval
  -- (CASE-FREE)
  Free vs e                  ->
    incrRenamingIndex (maxVar vs) >>= \renIndex ->
    let vs' = map (+ renIndex) vs
    in  peval $ Free vs' (Case ct (varSubst vs vs' e) bs)
  -- (CASE-OR)
  Or e1 e2                   -> peval (Or (Case ct e1 bs) (Case ct e2 bs))
  -- (CASE-CASE)
  Case ct' e@(Var _) bs'     -> peval (Case ct' e (subcase `onBranchExps` bs'))
    where subcase be = Case ct be bs
  -- (CASE-EVAL-3)
  Case _ _ _                 -> caseEval
  -- (CASE-TYPED)
  Typed e _                  -> peval (Case ct e bs)

 where caseEval = peval subj >>= \subj' ->
                  let case' = Case ct subj' bs in
                  if subj == subj' then return case' else peval case'

-- ---------------------------------------------------------------------------
-- Builtin functions
-- ---------------------------------------------------------------------------

--- partial evaluation of  built-in functions
peBuiltin :: QName -> [Expr] -> PEM Expr
peBuiltin f es
  | f == prelApply     = peBuiltInApply f es
  | f == prelCond      = peBuiltInCond  f es
  | f == prelCond'     = peBuiltInCond  f es
  | f == prelEq        = peBuiltinEq    f es
  | f == prelUni       = peBuiltinUni   f es
  | f == prelAmp       = peBuiltinCAnd  f es
  | f `elem` arithOps  = peBuiltinArith f es
  | otherwise          = return $ Comb FuncCall f es
  where arithOps = [ prelTimes, prelPlus, prelMinus
                   , prelLt, prelGt, prelLeq, prelGeq]

-- higher order application
peBuiltInApply :: QName -> [Expr] -> PEM Expr
peBuiltInApply f es = case es of
  [Comb ct@(ConsPartCall _) g es1, e2] -> peval $ addPartCallArg ct g es1 e2
  [Comb ct@(FuncPartCall _) g es1, e2] -> peval $ addPartCallArg ct g es1 e2
  [_                             , _ ] -> peArgs f es [1]
  _ -> error "PartEval.peBuiltInApply"

-- cond
peBuiltInCond :: QName -> [Expr] -> PEM Expr
peBuiltInCond f es = case es of
  [c, e] | delSQ c == trueExpr -> peval e
         | otherwise              -> peArgs f es [1]
  _ -> error "PartEval.peBuiltInCond"

-- Equality
peBuiltinEq :: QName -> [Expr] -> PEM Expr
peBuiltinEq f es = let es' = map delSQ es in case es' of
  [Lit l1              , Lit l2              ] -> return $ mkBool (l1 == l2)
  [Comb ConsCall c1 es1, Comb ConsCall c2 es2]
    | c1 == c2  -> peval $ mkConjunction f prelAnd trueExpr es1 es2
    | otherwise -> return falseExpr
  [_, _]
    | all (== trueExpr) es' -> return trueExpr
    | any (==  failedExpr) es' -> return failedExpr
    | otherwise                -> peArgs f es [1,2]
  _ -> error "PartEval.peBuiltinEq"

-- Unification
peBuiltinUni :: QName -> [Expr] -> PEM Expr
peBuiltinUni f es = let es' = map delSQ es in case es' of
  [Lit l1              , Lit l2              ]
    | l1 == l2  -> return trueExpr
    | otherwise -> return failedExpr
  [Comb ConsCall c1 es1, Comb ConsCall c2 es2]
    | c1 == c2  -> peval $ mkConjunction f prelAmp trueExpr es1 es2
    | otherwise -> return failedExpr
  [e1, e2]
    | all (== trueExpr) es' -> return trueExpr
    | any (==  failedExpr) es' -> return failedExpr
    | isVar e1                 -> unifyVar False f [e1,e2]
    | isVar e2                 -> unifyVar True  f [e2,e1]
    | otherwise                -> peArgs f es [1,2]
  _ -> error "PartEval.peBuiltinUni"

unifyVar :: Bool -> QName -> [Expr] -> PEM Expr
unifyVar flip f es = case es of
  [Var x, e] | dataExp e
            -> peval $ peBuiltinEqvarAux x e
             | flip
            -> peArgs f rev [1,2]
             | otherwise
            -> peArgs f es [1,2]
  _ -> error "PartEval.unifyVar"
 where rev = reverse es

-- currently we only consider literals and constants.. TO DO..
dataExp :: Expr -> Bool
dataExp (Var           _) = False
dataExp (Lit           _) = True
dataExp c@(Comb ct qn es) = case getSQ c of
  Just e -> dataExp e
  _      -> null es && (ct == ConsCall || qn == prelFailed)
dataExp (Free        _ _) = False
dataExp (Or          _ _) = False
dataExp (Case      _ _ _) = False
dataExp (Let         _ _) = False
dataExp (Typed       e _) = dataExp e

peBuiltinEqvarAux :: Int -> Expr -> Expr
peBuiltinEqvarAux x e = Case Flex (Var x) [subs2branches e]
  where
  subs2branches ex = case ex of
    Lit              c -> Branch (LPattern c)    trueExpr
    Comb ConsCall c [] -> Branch (Pattern  c []) trueExpr
    _                  -> error "PartEval.peBuiltinEqvarAux.subs2branches"
  --PENDING: extend function above to cover arbitrary data terms...

-- Concurrent conjunction
peBuiltinCAnd :: QName -> [Expr] -> PEM Expr
peBuiltinCAnd f es = case es of
  [e1, e2]  | e1' == trueExpr    -> peval e2
            | e2' == trueExpr    -> peval e1
            | e1' == failedExpr  -> return failedExpr
            | e2' == failedExpr  -> return failedExpr
            | otherwise          -> peArgs f es [1,2]
            where e1' = delSQ e1
                  e2' = delSQ e2
  _ -> error "PartEval.peBuiltinCAnd"

-- arithmetics
--extend to floats and to more operators..
peBuiltinArith :: QName -> [Expr] -> PEM Expr
peBuiltinArith f es = case es of
  [Lit (Intc i1), Lit (Intc i2)] -> return $ peArith i1 i2
  [_,_]                          -> peArgs f es [1,2]
  _ -> error "PartEval.peBuiltinArith"
 where
  peArith l1 l2
    | f == prelTimes = Lit (Intc (l1 * l2))
    | f == prelPlus  = Lit (Intc (l1 + l2))
    | f == prelMinus = Lit (Intc (l1 - l2))
    | f == prelLt    = mkBool (l1 <  l2)
    | f == prelGt    = mkBool (l1 >  l2)
    | f == prelLeq   = mkBool (l1 <= l2)
    | f == prelGeq   = mkBool (l1 >= l2)

-- Evaluate function arguments
peArgs :: QName -> [Expr] -> [Int] -> PEM Expr
peArgs f es is = case floatCase f [] zipped of
    Just e' -> return $ topSQ e'
    Nothing -> peEvalArgs f [] es
  where zipped = zipWith (\i e -> (i `elem` is, e)) [1..] es

floatCase :: QName -> [Expr] -> [(Bool, Expr)] -> Maybe Expr
floatCase _ _   []                    = Nothing
floatCase f les ((mayFloat, e) : ies) = case e of
    Case ct1 v@(Var _) bs | mayFloat ->
      Just $ Case ct1 v (subCase `onBranchExps` bs)
    _                                -> floatCase f (les ++ [e]) ies
  where subCase be = Comb FuncCall f (les ++ be : map snd ies)

peEvalArgs :: QName -> [Expr] -> [Expr] -> PEM Expr
peEvalArgs f les []     = return $ Comb FuncCall f les
peEvalArgs f les (e:es) = peval e >>= \new ->
  if e == new then peEvalArgs f (les ++ [e]) es
              else return $ topSQ $ Comb FuncCall f (les ++ new : es)

-- ---------------------------------------------------------------------------
-- Constructing expressions
-- ---------------------------------------------------------------------------

mkConjunction :: QName -> QName -> Expr -> [Expr] -> [Expr] -> Expr
mkConjunction eq con def es1 es2
  | null eqs  = def
  | otherwise = foldr1 (mkCall con) eqs
  where
  eqs = zipWith (mkCall eq) es1 es2
  mkCall f e1 e2 = Comb FuncCall f [e1, e2]

mkBool :: Bool -> Expr
mkBool True  = trueExpr
mkBool False = falseExpr
