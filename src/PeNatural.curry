--- --------------------------------------------------------------------------
--- This module performs the partial evaluation of a given expression,
--- based on the natural semantics defined in [NatSem] and improved
--- by Peemöller and Skrlac [PS14].
---
--- To ensure termination, the partial evaluator performs *at most one*
--- unfolding step, where an unfolding step is either
---
---  * The replacement of a function call with the function's body
---    (and an appropiate substitution for the function's variables)
---  * The replacement of a bound variable by the value of the expression
---    it is bound to. This is necessary to ensure termination for situations
---    like in @let ones = 1 : ones in ones@ where unbound replacements
---    would result in non-termination.
---
--- @author Björn Peemöller
--- @version September 2015
--- --------------------------------------------------------------------------
module PeNatural (pevalExpr) where

import           AnsiCodes (cyan, magenta)
import           List      ((\\), elemIndex, find, intersect, isPrefixOf, nub)
import           Maybe     (fromMaybe, isNothing)
import qualified Set       (Set, elem, empty, insert, null)

import Text.Pretty       (pPrint)
import FlatCurry.Types
import FlatCurryGoodies
import FlatCurryPretty   (ppExp)
import Heap as H
import NDState
import Normalization     (freshRule, simplifyExpr, normalizeFreeExpr)
import Output            (assert, colorWith, debug, traceDetail)
import PevalOpts         (Options (..), ProceedMode (..))
import Subst             (mkSubst, subst)
import Utils             (count, indentStr)

--- Partial evaluation.
--- It is crucial that the free variables of the input expression directly
--- correspond to the free variables of the output expression, as these may
--- later be substituted by concrete arguments.
---
--- @param opts - Global parameters, used for en/disabling tracing.
--- @param p    - Program
--- @param e    - expression to be evaluated
--- @return partially evaluated expression `e'`
pevalExpr :: Options -> Prog -> Expr -> Expr
pevalExpr opts p e = assert (all (< 0) fvs) "PeNatural.pevalExpr"
                   $ normalizeFreeExpr $ simplifyExpr $ flatND e'
  where
  fvs   = freeVars e
  e'    = runState (nf e) (initState opts p fresh)
  fresh = - (length fvs + 1)

--- Flatten the expression/state-pair by extracting the heap from the state
--- and integrating referenced bindings into the expression.
--- In addition, failures directly below non-determinism are eliminated.
flatND :: Result (Expr, PEState) -> Expr
flatND (Return (e, s)) = H.dereference (pesHeap s) e
flatND (Choice    a b) = mkOr (flatND a) (flatND b)

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

--- Internal state of the partial evaluation, consisting of
---   * The list of all defined function declarations,
---     used for unfolding (read-only).
---   * The heap containing variable bindings.
---   * An index for fresh variables.
---   * The number of already performed unfolding operations,
---     used for local termination.
---   * The depth of nested calls to @peval@, used for trace layouting.
data PEState = PEState
  { pesOptions  :: Options
  , pesTypes    :: [TypeDecl]
  , pesFuncs    :: [FuncDecl]
  , pesHeap     :: H.Heap
  , pesFresh    :: VarIndex
  , pesUnfolded :: Set.Set QName
  , pesTrace    :: Int
  }

--- Initial state for partial evaluation.
initState :: Options -> Prog -> Int -> PEState
initState opts (Prog _ _ ts fs _) fresh = PEState
  { pesOptions  = opts
  , pesTypes    = ts
  , pesFuncs    = fs
  , pesHeap     = emptyHeap
  , pesFresh    = fresh
  , pesUnfolded = Set.empty
  , pesTrace    = 1
  }

--- Partial evaluation monad.
type PEM a = State PEState a

--- Retrieve the global options.
getOpts :: PEM Options
getOpts = getsS pesOptions

--- Compute if the given function is not user defined.
isNotUserDefined :: QName -> PEM Bool
isNotUserDefined f = isNothing <$> lookupRule f

--- Lookup a function rule.
lookupRule :: QName -> PEM (Maybe ([VarIndex], Expr))
lookupRule f
  = getsS (find (hasName f) . pesFuncs) >+= \mbFunc ->
    returnS $ case mbFunc of
      Just (Func _ _ _ _ (Rule vs rhs)) -> Just (vs, rhs)
      _                                 -> Nothing

--- Try to lookup all constructors of a data type.
getAllCons :: QName -> PEM (Maybe [QName])
getAllCons c = getsS (gac . pesTypes)
  where
  gac []                      = Nothing
  gac (t : tds) | c `elem` cs = Just cs
                | otherwise   = gac tds
    where cs = constructors t

--- Trace a monadic action.
traceM :: String -> PEM ()
traceM msg = getOpts  >+= \opts ->
             getTrace >+= \t    ->
             returnS (traceDetail opts (indentStr (2 * t) msg) ())

--- assert a monadic truth value.
assertM :: PEM Bool -> String -> PEM ()
assertM check msg = getOpts >+= \opts -> case optAssert opts of
  True  -> check >+= \b -> returnS $ assert b msg ()
  False -> returnS ()

--- Retrieve the next fresh variable.
freshVar :: PEM VarIndex
freshVar = getS >+= \s -> let fresh = pesFresh s in
           putS s { pesFresh = fresh - 1 } >+ returnS fresh

--- Get current nesting depth.
getTrace :: PEM Int
getTrace = getsS pesTrace

--- Nest the next invocation by increasing the nesting depth.
nestTrace :: PEM a -> PEM a
nestTrace act = modifyS (\s -> s { pesTrace = pesTrace s + 1 }) >+
                act >+= \res ->
                modifyS (\s -> s { pesTrace = pesTrace s - 1 }) >+
                returnS res

--- Retrive the set of already unfolded functions.
getUnfolded :: PEM (Set.Set QName)
getUnfolded = getsS pesUnfolded

--- Add a function to the set of already unfolded functions.
addUnfolded :: QName -> PEM ()
addUnfolded f = modifyS (\s -> s { pesUnfolded = Set.insert f (pesUnfolded s) })

--- Check if we can proceed unfolding a function call.
proceed :: QName -> PEM Bool
proceed f =
  getUnfolded >+= \set  ->
  getOpts     >+= \opts ->
  case optProceedMode opts of
    PMNone                  ->                  returnS False
    PMOne  | Set.null set   -> addUnfolded f >+ returnS True
           | otherwise      ->                  returnS False
    PMEach | Set.elem f set ->                  returnS False
           | otherwise      -> addUnfolded f >+ returnS True
    PMAll                   ->                  returnS True

--- Get the current heap.
getHeap :: PEM Heap
getHeap = getsS pesHeap

--- Modify the current heap.
modifyHeap :: (Heap -> Heap) -> PEM ()
modifyHeap f = modifyS $ \s -> s { pesHeap = f (pesHeap s) }

--- Get the binding for a variable in the current heap.
getBinding :: VarIndex -> PEM Binding
getBinding v = H.getHeap v <$> getHeap

--- Bind a variable to marker for a black hole.
bindHole :: VarIndex -> PEM ()
bindHole v = assert (v < 0) "PeNatural.bindHole: positive variable"
           $ modifyHeap (H.bindHole v)

--- Bind a variable to an expression.
bindE :: VarIndex -> Expr -> PEM ()
bindE v e = assert (v < 0) "PeNatural.bindE: positive variable"
          $ modifyHeap (H.bindExpr v e)

--- Bind a variable to a marker for a free variable.
bindF :: VarIndex -> PEM ()
bindF v = assert (v < 0) "PeNatural.bindF: positive variable"
        $ modifyHeap (H.bindFree v)

--- Bind a variable to a marker for a unbound parameter variable.
bindP :: VarIndex -> PEM ()
bindP v = assert (v < 0) "PeNatural.bindP: positive variable"
        $ modifyHeap (H.bindParam v)

--- Bind a variable lazily to an expression.
bindLE :: VarIndex -> Expr -> PEM ()
bindLE v e = assert (v < 0) "PeNatural.bindLE: positive variable"
           $ modifyHeap (H.bindLazyExpr v e)

--- Bind a variable lazily to a marker for a free variable.
bindLF :: VarIndex -> PEM ()
bindLF v = assert (v < 0) "PeNatural.bindLF: positive variable"
           $ modifyHeap (H.bindLazyFree v)

--- Bind a variable lazily to a marker for a unbound parameter variable.
bindLP :: VarIndex -> PEM ()
bindLP v = assert (v < 0) "PeNatural.bindLP: positive variable"
           $ modifyHeap (H.bindLazyParam v)

--- Bind the argument of a constructor or function call.
--- When the argument is *not* a variable, i.e., the call is not flattened,
--- a new (fresh) variable is generated, bound to the expression in the heap
--- and returned as the argument. In addition, if an expression is a declaration
--- of free variables or a let expressions the corresponding bindings are
--- directly performed in the heap to prevent nested let expressions and alike
--- in the heap.
--- In consequence, this operation performs flattening "on-the-fly".
bindArg :: Expr -> PEM Expr
bindArg e = case e of
  Var _      -> returnS e
  Let  ds e' -> addBindings ds e' >+= bindArg
  Free vs e' -> addFrees    vs e' >+= bindArg
  _          -> freshVar >+= \v -> bindE v e >+ returnS (Var v)

--- Bind a new (fresh) variable in the heap as free and return it.
bindFree :: PEM Expr
bindFree = freshVar >+= \v -> bindF v >+ returnS (Var v)

-- ---------------------------------------------------------------------------
-- The actual partial evaluation
-- ---------------------------------------------------------------------------

--- Partial evaluation to *head normal form*. This does the same as peval,
--- but performs some additional compressions afterwards.
nf :: Expr -> PEM Expr
nf e = peval e >+= nf'
 where
  nf' e' = case e' of
    Comb ct qn es | isConsCall ct -> Comb ct qn <$> mapS nf' es
                  | otherwise     -> case getSQ e' of
      Just de -> nf' de >+= defer
      _       -> defer e'
    Case ct ce bs                 -> Case ct ce <$> mapS (peBranch ce) bs
    _                             -> defer e'

  peBranch e' b = freshBranch b >+= eval
    where
      eval (Branch p' be') = Branch p' <$> nested (case e' of
        Var x -> bindE x (pat2exp p') >+ peval (delSQ be') >+= nf'
        _     ->                         peval (delSQ be') >+= nf')

--- Perform a nested partial evaluation. Nested evaluations are performed
--- for expressions below case expressions and, thus, may not alter existing
--- bindings in the heap. Therefore, the heap is restricted to values only.
--- Although nested expressions would have been evaluated later (depending on
--- the abstraction), this is useful to evaluate some trivial expressions
--- like pattern matching on a constructor, which results in smaller expressions
--- and thus may improve termination and abstraction behaviour.
nested :: PEM Expr -> PEM Expr
nested act =
  getS >+= \s ->
  let s' = s { pesOptions = (pesOptions s) { optProceedMode = PMNone } }
  in  defer $ flatND $ runState nestedAct s'
  where
  nestedAct = modifyHeap restrictToValues >+
              getHeap >+= \h  ->
              act     >+= \e' ->
              modifyHeap (\h' -> h' `H.without` h) >+
              returnS e'
  restrictToValues h = [ b | b@(_, BoundVar e) <- h, isValue e]
    where
    isValue e = case e of
      Var _                     -> True
      Lit _                     -> True
      Comb ConsCall         _ _ -> True
      Comb (ConsPartCall _) _ _ -> True
      Comb (FuncPartCall _) _ _ -> True
      Comb FuncCall         _ _ -> isSQ e || isFailed e
      _                         -> False

--- Partial evaluation to *head normal form*.
---
--- /Note:/ This function implements tracing and assertions for debugging
--- purposes, the real work in done in function `hnf`.
peval :: Expr -> PEM Expr
peval e =
  getOpts >+= \opts ->
  getHeap >+= \h1   -> traceM (showConfig opts h1 e)              >!
  nestTrace (hnf e)                                               >+= \v  ->
  getHeap >+= \h2   -> traceM (showConfig opts h2 v)              >!
  assertM (returnS $ noShadowing v) ("Shadowing\n" ++ showExpr   opts    v) >!
  assertM (isResidualValue       v) ("No value\n"  ++ showConfig opts h2 v) >!
  returnS v
  where
  showHeap   opts h   = colorWith opts magenta (pPrint $ ppHeap h)
  showExpr   opts   x = colorWith opts cyan    (pPrint $ ppExp  x)
  showConfig opts h x = showHeap opts h ++ " : " ++ showExpr opts x

--- Compute if the given expression forms a *residual value*, the result of
--- partial evaluation. This may serve as an assertion for the correctness of
--- the implementation.
--- A residual value is is one of the following:
---   * a variable $x$ where $x$ is either free, unbound, or bound to an
---     expression not further evaluable
---     (typically an external function or a residual case expression)
---   * a literal $l$
---   * a constructor term $c(x_n)$, where $x_n$ are variables
---   * a call to `failed`
---   * a partial function call $f(x_k)$, where $x_k$ are variables
---   * a residual case-expression $(f)case e' of { p_n -> v_n }$
---     where $e'$ is not evaluable and $v_n$ are values
---   * a suspended case-expression $case x of { p_n -> v_n }$
---     where $x$ is a free variable and $v_n$ are values
---   * a square bracketed expression $[|e|]$ designated for further evaluation,
---     where $e$ does not contain further square brackets.
isResidualValue :: Expr -> PEM Bool
isResidualValue e = case e of
  Var _                 -> notEvaluable e
  Lit _                 -> returnS True
  Comb FuncCall _ _
    | isFailed e      -> returnS True
    | otherwise       -> case getSQ e of
      Just e' -> returnS $ not $ hasSQ e'
      _       -> notEvaluable e
  Comb _                _ es -> allS isNestedValue es
    where isNestedValue x = (isVar x ||) <$> isResidualValue x
  Case ct (Var x) bs -> getBinding x >+= \bdg -> case bdg of
    BoundVar  b            -> notEvaluable b
    LazyBound b            -> notEvaluable b
    FreeVar  | ct == Rigid -> allS isResidualValue (branchExprs bs)
    LazyFree | ct == Rigid -> allS isResidualValue (branchExprs bs)
    Param                  -> allS isResidualValue (branchExprs bs)
    LazyParam              -> allS isResidualValue (branchExprs bs)
    _                      -> returnS False
  Case _  e'      bs -> (&&) <$> notEvaluable e'
                             <*> allS isResidualValue (branchExprs bs)
  Or e1 e2           -> (&&) <$> isResidualValue e1 <*> isResidualValue e2
  _                  -> returnS False
 where
  notEvaluable e' = case e' of
    Var x               -> getBinding x >+= \bdg -> case bdg of
      BlackHole   -> returnS False
      BoundVar  b -> notEvaluable b
      LazyBound b -> notEvaluable b
      FreeVar     -> returnS True
      LazyFree    -> returnS True
      Param       -> returnS True
      LazyParam   -> returnS True
    Comb FuncCall f es
      | isFailed e' || isSQ e' -> returnS True
      | otherwise              -> (&&) <$> isNotUserDefined f <*>
                                  ((not (isBuiltin f) ||) <$> anyS notEvaluable es)
    Case _ s _          -> notEvaluable s
    _                   -> returnS False

--- Defer the partial evaluation of an expression for later.
--- This implementation is smart in that it ignores obviously residual code,
--- e.g., constructor calls, non-determinism etc.
defer :: Expr -> PEM Expr
defer e = case e of
  Var x                     -> getBinding x >+= \bdg -> case bdg of
    BlackHole               -> failS
    FreeVar                 -> returnS e
    LazyFree                -> returnS e
    Param                   -> returnS e
    LazyParam               -> returnS e
    _                       -> returnS $ topSQ e
  Lit _                     -> returnS e
  Comb FuncCall         _ _
      | isFailed e   -> returnS e
      | otherwise           -> returnS $ topSQ e
  Comb (FuncPartCall _) _ _ -> returnS $ topSQ e
  Comb ct qn es             -> Comb ct qn <$> mapS defer es
  Case ct (Var x) bs        -> getBinding x >+= \bdg -> case bdg of
    BlackHole               -> failS
    FreeVar   | ct == Rigid -> Case ct (Var x) <$> onBranchS defer bs
    LazyFree  | ct == Rigid -> Case ct (Var x) <$> onBranchS defer bs
    Param                   -> Case ct (Var x) <$> onBranchS defer bs
    LazyParam               -> Case ct (Var x) <$> onBranchS defer bs
    _                       -> returnS $ topSQ e
  _                         -> returnS $ topSQ e

--- Add a list of bindings to the heap using fresh variables.
addBindings :: [(VarIndex, Expr)] -> Expr -> PEM Expr
addBindings ds e =
  mapS (\_ -> freshVar) ds >+= \ys ->
  let (xs, es) = unzip ds
      sigma    = mkSubst xs (map Var ys) in
  mapS_ (uncurry bindE) (zip ys (map (subst sigma) es)) >+
  returnS (subst sigma e)

--- Add a list of free variables to the heap using fresh variables.
addFrees :: [VarIndex] -> Expr -> PEM Expr
addFrees xs e =
  mapS (\_ -> bindFree) xs >+= \ys ->
  returnS (subst (mkSubst xs ys) e)

--- Add a list of free variables to the heap using fresh variables.
freshBranch :: BranchExpr -> PEM BranchExpr
freshBranch b@(Branch p be) = case p of
  LPattern _    -> returnS b
  Pattern  c xs -> mapS (\_ -> freshVar) xs >+= \ys ->
                   returnS $ Branch (Pattern c ys)
                             (subst (mkSubst xs (map Var ys)) be)

--- Partial evaluation of an expression to its head normal form.
hnf :: Expr -> PEM Expr
hnf (Var          x) = peVar  x
hnf (Lit          l) = peLit  l
hnf c@(Comb ct f es) = case getSQ c of
  Just e -> defer e
  _      -> peComb ct f es
hnf (Let       ds e) = peLet  ds e
hnf (Free      vs e) = peFree vs e
hnf (Or       e1 e2) = peOr   e1 e2
hnf (Case   ct e bs) = peCase ct bs e
hnf (Typed      e _) = peval  e

--- Partial evaluation of a variable.
--- Evaluation of a variable may only result in the following values:
---  * A failure (failed)
---  * A variable, either free or unbound (parameter)
---  * A deferred variable
---  * A literal
---  * A constructor call
---  * A partial call
---  * An expression that can not be further evaluated
peVar :: VarIndex -> PEM Expr
peVar x = getBinding x >+= \bdg -> case bdg of
  -- black hole -> failure
  BlackHole   -> failS
  -- free variable
  FreeVar     -> returnS varx
  LazyFree    -> returnS varx
  -- unbound variable
  Param       -> returnS varx
  LazyParam   -> returnS varx
  -- expression -> evaluate
  BoundVar  e -> bindHole x >+ peval e >+= bindAndCheck
  LazyBound e -> bindHole x >+ peval e >+= bindAndCheckLazy
 where
  varx = Var x
  -- update binding and compute result value
  bindAndCheck v = bindE x v >+ case v of
    -- free or unbound variable
    Var  _                         -> returnS v
    -- literal
    Lit  _                         -> returnS v
    -- failed
    Comb FuncCall _ _ | isFailed v -> returnS v
    -- deferred expression
                      | isSQ     v -> defer   varx
    -- primitives that could not be evaluated, such as x + x where x is unbound
                      | otherwise  -> returnS varx
    -- constructor or partial application
    Comb _        _ _              -> returnS v
    -- residual case
    Case _ _ _                     -> returnS varx
    -- there are no other values allowed
    _  -> error $ "PeNatural.bindAndCheck: " ++ show v

  -- Update lazy binding and compute result value.
  -- TODO: Missing documentation
  bindAndCheckLazy v = case v of
    Var w                  -> bindLE x v >+ getBinding w >+= \bdg -> case bdg of
      BlackHole            -> failS
      BoundVar e           -> bindLE w e >+ returnS v
      FreeVar              -> bindLF w   >+ returnS v
      Param                -> bindLP w   >+ returnS v
      _                    -> returnS v
    -- literal
    Lit _                  -> bindE x v >+ returnS v
    Comb FuncCall _ _
      -- failed
      | isFailed v         -> bindE  x v >+ returnS v
      -- deferred expression
      | isSQ     v         -> bindLE x v >+ defer   varx
      -- residual primitive
      | otherwise          -> bindLE x v >+ returnS varx
    -- constructor or partial application
    Comb ct qn xs          -> mapS  (const freshVar) xs >+= \ys ->
                              let val = Comb ct qn (map Var ys) in
                              mapS_ (uncurry bindLE) (zip ys xs) >+
                              bindE x val >+ returnS val
    -- residual case
    Case _ _ _             -> bindLE x v >+ returnS varx
    -- there are no other values allowed
    _                      -> error $ "PeNatural.bindAndCheckLazy: " ++ show v


--- Partial evaluation of a literal.
--- A literal evaluates to itself.
peLit :: Literal -> PEM Expr
peLit l = returnS (Lit l)

--- Partial evaluation of a combination.
--- For constructor calls and partial calls, the arguments are bound to fresh
--- variables and the constructor/partial call is returned.
--- For function calls, it is decided whether the function is builtin or
--- user-defined, and then evaluated.
peComb :: CombType -> QName -> [Expr] -> PEM Expr
peComb ct f es = case ct of
  FuncCall       -> lookupRule f >+= \mbRule -> case mbRule of
    Nothing      -> peBuiltin   f es
    Just (xs, e) -> peRule xs e f es
  _              -> Comb ct f <$> mapS bindArg es

--- Partial evaluation of a user-defined function.
--- These are checked if further evaluations should be performed, and either
--- deferred or unfolded.
peRule :: [VarIndex] -> Expr -> QName -> [Expr] -> PEM Expr
peRule xs e f es = proceed f >+= \allowed -> if allowed
  then unfold xs e es >+= peval
  else defer (func f es)

--- Bind expressions to (fresh variants of) the given variables and evaluate
--- the corresponding expression. This is useful for function calls and
--- branches of case expressions.
unfold :: [VarIndex] -> Expr -> [Expr] -> PEM Expr
unfold xs e es =
--   = mapS bindArg es >+= \ys -> returnS (subst (mkSubst xs ys) e)
  mapS bindArg' (zip xs' es) >+= \es' ->
  returnS (subst (mkSubst xs' es') e')
 where
  Rule xs' e' = freshRule (maximumVarIndex es + 1) (Rule xs e)
  bindArg' (x, b)
    | count x (freeVarsDup e') <= 1 || isConstrTerm b = returnS b
    | otherwise                                       = bindArg b

--- Partial evaluation of a let expression.
peLet :: [(VarIndex, Expr)] -> Expr -> PEM Expr
peLet ds e = addBindings ds e >+= peval

--- Partial evaluation of free variables.
peFree :: [VarIndex] -> Expr -> PEM Expr
peFree xs e = addFrees xs e >+= peval

--- Partial evaluation of non-deterministic choice.
peOr ::  Expr -> Expr -> PEM Expr
peOr e1 e2 = peval e1 <|> peval e2

--- Partial evaluation of case expressions.
peCase :: CaseType -> [BranchExpr] -> Expr -> PEM Expr
peCase ct bs subj = peval subj >+= \v -> case v of
  Var x               -> getBinding x >+= \bdg -> case bdg of
    BlackHole               -> failS
    FreeVar   | ct == Flex  -> narrowCase
    LazyFree  | ct == Flex  -> narrowCase
    _                       -> deferCase v
   where
    narrowCase = foldr choiceS failS $ map guess bs

    guess (Branch (LPattern   l) be) = bindE x (Lit l)              >+ peval be
    guess (Branch (Pattern c xs) be) = mapS (\_ -> bindFree) xs     >+= \ys ->
                                       bindE x (Comb ConsCall c ys) >+
                                       peval (subst (mkSubst xs ys) be)

    liftSubCase d e as  = mapS freshBranch as >+= \as' ->
                          mkCase d e <$> onBranchS defer
                          [ Branch p (subcase v) | Branch p _ <- as']

  Lit l               -> case findBranch (LPattern l) bs of
    Nothing      -> failS
    Just ( _, e) -> peval e

  Comb ConsCall c es  -> case findBranch (Pattern c []) bs of
    Nothing      -> failS
    Just (xs, e) -> unfold xs e es >+= peval

  Comb _ _ _
    | v == failedExpr -> failS
    | otherwise       -> case getSQ v of
        -- This is an important *optimization* for headPerm to avoid
        -- the construction of multiply nested case expressions.
        -- May become obsolete if normalization implements case-of-case
      Just (Case d e as) -> mapS freshBranch as >+= \as' ->
                            defer (mkCase d e (subcase `onBranchExps` as'))
      Just e             -> defer (mkCase ct e bs)
      _                  -> deferCase v -- unevaluable function (primitive)

  Case d e as         -> mapS freshBranch as >+= \as' ->
                         peval (mkCase d e (subcase `onBranchExps` as'))

  _                   -> error $ "PeNatural.peCase: " ++ show v
 where subcase  be = mkCase ct be bs
       deferCase v = mkCase ct v <$> onBranchS defer bs

--- Perform a monadic action on all expressions of the branches.
onBranchS :: (Expr -> PEM Expr) -> [BranchExpr] -> PEM [BranchExpr]
onBranchS f = mapS (\(Branch p e) -> Branch p <$> f e)

--- Failing derivation.
failS :: PEM Expr
failS = returnS failedExpr

--- Result 'True'.
succeedS :: PEM Expr
succeedS = returnS trueExpr

-- ---------------------------------------------------------------------------
-- Partial Evaluation of Builtin Functions
-- ---------------------------------------------------------------------------

--- partial evaluation of built-in functions.
peBuiltin :: QName -> [Expr] -> PEM Expr
peBuiltin f es
  | f == prelFailed    = peBuiltinFailed         f es
  | f == prelSuccess   = peBuiltinSuccess        f es
  | f == prelUnknown   = peBuiltinUnknown        f es
  | f == prelChoice    = binary peBuiltinChoice  f es
  | f == prelAmp       = binary peBuiltinAmp     f es
  | f == prelCond      = binary peBuiltInCond    f es
  | f == prelCond'     = binary peBuiltInCond    f es
  | f == prelApply     = binary peBuiltInApply   f es
  | f == prelPlus      = binary peBuiltinPlus    f es
  | f == prelMinus     = binary peBuiltinMinus   f es
  | f == prelTimes     = binary peBuiltinTimes   f es
  | f == prelDiv       = binary peBuiltinDiv     f es
  | f == prelMod       = binary peBuiltinMod     f es
  | f == prelAnd       = binary peBuiltinAnd     f es
  | f == prelOr        = binary peBuiltinOr      f es
  | f == prelEq        = binary peBuiltinEq      f es
  | f == prelNeq       = binary peBuiltinNeq     f es
  | f `elem` orderOps  = binary peBuiltinOrder   f es
  | f == prelUni       = binary peBuiltinUni     f es
  | f == prelLazyUni   = binary peBuiltinLazyUni f es
  | otherwise          = func f <$> mapS peval es
  where orderOps = [prelLt, prelLeq, prelGt, prelGeq]

--- Is a function builtin into the partial evaluator?
isBuiltin :: QName -> Bool
isBuiltin f = f `elem` builtin
  where
  builtin = [ prelFailed, prelSuccess, prelUnknown, prelChoice
            , prelAmp, prelCond, prelCond', prelApply
            , prelPlus, prelMinus, prelTimes, prelDiv, prelMod
            , prelEq, prelNeq, prelLt, prelLeq, prelGt, prelLeq
            , prelUni, prelLazyUni
            ]

--- Auxiliary function for pattern matching arguments of binary functions.
binary :: (QName -> Expr -> Expr -> PEM a) -> QName -> [Expr] -> PEM a
binary act qn es = case es of
  [e1, e2] -> act qn e1 e2
  _        -> error $ "PeNatural.binary: " ++ show qn

--- Partial evaluation of `failed`.
peBuiltinFailed :: QName -> [Expr] -> PEM Expr
peBuiltinFailed _ es = case es of
  [] -> failS
  _  -> error "PeNatural.peBuiltinFailed"

--- Partial evaluation of `success`.
peBuiltinSuccess :: QName -> [Expr] -> PEM Expr
peBuiltinSuccess _ es = case es of
  [] -> succeedS
  _  -> error "PeNatural.peBuiltinSuccess"

--- Partial evaluation of `unknown`.
peBuiltinUnknown :: QName -> [Expr] -> PEM Expr
peBuiltinUnknown _ es = case es of
  [] -> bindFree
  _  -> error "PeNatural.peBuiltinUnknown"

--- Partial evaluation of `(?)`.
peBuiltinChoice :: QName -> Expr -> Expr -> PEM Expr
peBuiltinChoice _ e1 e2 = peOr e1 e2

--- Partial evaluation of concurrent conjunction `(&)`.
--- Suspends on free variables.
peBuiltinAmp :: QName -> Expr -> Expr -> PEM Expr
peBuiltinAmp f e1 e2 = peval e1 >+= \v1 -> case v1 of
  Var x                 -> getBinding x >+= \bdg -> case bdg of
    FreeVar        -> bindE x trueExpr >+ peval e2
    LazyFree       -> bindE x trueExpr >+ peval e2
    _              -> other v1
  _ | v1 == trueExpr -> peval e2
    | otherwise         -> other v1
 where
  other v1 = parDefault cont v1 e2 $ peval e2 >+= \v2 -> case v2 of
    _ | v2 == trueExpr -> peval v1
      | otherwise         -> parDefault (flip cont) v2 v1 (returnS $ cont v1 v2)
  cont x y = func f [x, y]

--- Partial evaluation of conditional expressions `(&>)`.
--- Suspends if the first argument is a free variable.
peBuiltInCond :: QName -> Expr -> Expr -> PEM Expr
peBuiltInCond _ e1 e2 = peval e1 >+= \v1 -> case v1 of
  Comb FuncCall g [a, b]
      -- replace `(e1 &> e2) &> e3` by `e1 &> (e2 &> e3)`
    | g == prelCond      -> peval $ func prelCond [a, func prelCond [b, e2]]
    | g == prelCond'     -> peval $ func prelCond [a, func prelCond [b, e2]]
  _ | v1 == trueExpr  -> peval e2
    | otherwise          -> seqDefault cont v1 e2 (returnS $ cont v1 e2)
 where cont x y = func prelCond [x, y]

--- Partial evaluation of higher order application.
--- Suspends if the first argument is a free variable.
peBuiltInApply :: QName -> Expr -> Expr -> PEM Expr
peBuiltInApply f e1 e2 = peval e1 >+= \v1 -> case v1 of
  Comb c g es | isPartCall c -> peval $ addPartCallArg c g es e2
  _                          -> seqDefault cont v1 e2 (returnS $ cont v1 e2)
 where cont x y = func f [x, y]

--- Partial evaluation of addition.
--- Suspends on free variables.
peBuiltinPlus :: QName -> Expr -> Expr -> PEM Expr
peBuiltinPlus f e1 e2 = peval e1 >+= \v1 ->
                        peval e2 >+= \v2 -> case (v1, v2) of
    (Lit (Intc  0),            _ ) -> returnS v2
    (_            , Lit (Intc  0)) -> returnS v1
    (Lit (Intc l1), Lit (Intc l2)) -> returnS (Lit (Intc (l1 + l2)))
    _                              -> parDefault cont        v1 v2
                                    $ parDefault (flip cont) v2 v1
                                      (returnS $ cont v1 v2)
  where cont x y = func f [x, y]

--- Partial evaluation of subtraction.
--- Suspends on free variables.
peBuiltinMinus :: QName -> Expr -> Expr -> PEM Expr
peBuiltinMinus f e1 e2 = peval e1 >+= \v1 ->
                         peval e2 >+= \v2 -> case (v1, v2) of
    (_            , Lit (Intc  0)) -> returnS v1
    (Lit (Intc l1), Lit (Intc l2)) -> returnS (Lit (Intc (l1 - l2)))
    _                              -> parDefault cont        v1 v2
                                    $ parDefault (flip cont) v2 v1
                                      (returnS $ cont v1 v2)
  where cont x y = func f [x, y]

--- Partial evaluation of multiplication.
--- Suspends on free variables.
peBuiltinTimes :: QName -> Expr -> Expr -> PEM Expr
peBuiltinTimes f e1 e2 = peval e1 >+= \v1 ->
                         peval e2 >+= \v2 -> case (v1, v2) of
    (Lit (Intc  1),            _ ) -> returnS v2
    (_            , Lit (Intc  1)) -> returnS v1
    (Lit (Intc l1), Lit (Intc l2)) -> returnS (Lit (Intc (l1 * l2)))
    _                              -> parDefault cont        v1 v2
                                    $ parDefault (flip cont) v2 v1
                                      (returnS $ cont v1 v2)
  where cont x y = func f [x, y]

--- Partial evaluation of division.
--- Suspends on free variables.
peBuiltinDiv :: QName -> Expr -> Expr -> PEM Expr
peBuiltinDiv f e1 e2 = peval e1 >+= \v1 ->
                       peval e2 >+= \v2 -> case (v1, v2) of
    (_            , Lit (Intc  0)) -> failS
    (_            , Lit (Intc  1)) -> returnS v1
    (Lit (Intc l1), Lit (Intc l2)) -> returnS (Lit (Intc (div l1 l2)))
    _                              -> parDefault cont        v1 v2
                                    $ parDefault (flip cont) v2 v1
                                      (returnS $ cont v1 v2)
  where cont x y = func f [x, y]

--- Partial evaluation of modulo.
--- Suspends on free variables.
peBuiltinMod :: QName -> Expr -> Expr -> PEM Expr
peBuiltinMod f e1 e2 = peval e1 >+= \v1 ->
                       peval e2 >+= \v2 -> case (v1, v2) of
    (_            , Lit (Intc  0)) -> failS
    (_            , Lit (Intc  1)) -> returnS (Lit (Intc 0          ))
    (Lit (Intc l1), Lit (Intc l2)) -> returnS (Lit (Intc (mod l1 l2)))
    _                              -> parDefault cont        v1 v2
                                    $ parDefault (flip cont) v2 v1
                                      (returnS $ cont v1 v2)
  where cont x y = func f [x, y]

--- Partial evaluation of `(&&)`.
--- It is only builtin because it is needed for equality and ordering.
peBuiltinAnd :: QName -> Expr -> Expr -> PEM Expr
peBuiltinAnd f e1 e2 = peRule [1, 2] e f [e1, e2]
  where e = Case Flex (Var 1)
              [ Branch (Pattern prelFalse []) falseExpr
              , Branch (Pattern prelTrue  []) (Var 2)
              ]

--- Partial evaluation of `(||)`.
--- It is only builtin because it is needed for equality and ordering.
peBuiltinOr :: QName -> Expr -> Expr -> PEM Expr
peBuiltinOr f e1 e2 = peRule [1, 2] e f [e1, e2]
  where e = Case Flex (Var 1)
              [ Branch (Pattern prelFalse []) (Var 2)
              , Branch (Pattern prelTrue  []) trueExpr
              ]

--- Eventually defer the call to builtin functions.
pevalOrDefer :: QName -> Expr -> PEM Expr
pevalOrDefer f e = proceed f >+= \p -> if p then peval e else defer e

--- Partial evaluation of equality.
--- Suspends on free variables.
peBuiltinEq :: QName -> Expr -> Expr -> PEM Expr
peBuiltinEq f e1 e2 = peval e1 >+= \v1 -> peval e2 >+= \v2 -> case (v1, v2) of
  (Lit l1              , Lit l2              ) -> returnS $ mkBool (l1 == l2)
  (Comb ConsCall c1 es1, Comb ConsCall c2 es2)
    | c1 == c2                      -> pevalOrDefer f
                                     $ combine f prelAnd trueExpr es1 es2
    | otherwise                     -> returnS falseExpr
  _ | all (== trueExpr) [v1, v2] -> returnS trueExpr
    | otherwise                     -> parDefault cont        v1 v2
                                     $ parDefault (flip cont) v2 v1
                                       (returnS $ cont v1 v2)
 where cont x y = func f [x, y]

--- Partial evaluation of inequality.
--- Suspends on free variables.
peBuiltinNeq :: QName -> Expr -> Expr -> PEM Expr
peBuiltinNeq f e1 e2 = peval e1 >+= \v1 -> peval e2 >+= \v2 -> case (v1, v2) of
  (Lit l1              , Lit l2              ) -> returnS $ mkBool (l1 /= l2)
  (Comb ConsCall c1 es1, Comb ConsCall c2 es2)
    | c1 == c2                      -> pevalOrDefer f
                                     $ combine f prelOr falseExpr es1 es2
    | otherwise                     -> returnS trueExpr
  _ | all (== trueExpr) [v1, v2] -> returnS falseExpr
    | otherwise                     -> parDefault cont        v1 v2
                                     $ parDefault (flip cont) v2 v1
                                       (returnS $ cont v1 v2)
 where cont x y = func f [x, y]

--- Partial evaluation of order operations ((<), (>), (<=), (>=)).
--- Suspends on free variables.
peBuiltinOrder :: QName -> Expr -> Expr -> PEM Expr
peBuiltinOrder f e1 e2 = peval e1 >+= \v1 ->
                         peval e2 >+= \v2 -> case (v1, v2) of
  (Lit l1              , Lit l2              ) -> returnS $ peLitOrder f l1 l2
  (Comb ConsCall c1 es1, Comb ConsCall c2 es2) ->
    getAllCons c1 >+= \mbCons -> case mbCons of
      Nothing -> returnS $ cont v1 v2
      Just cs -> pevalOrDefer f $ peConsOrder f cs c1 c2 es1 es2
  _ -> parDefault cont v1 v2
     $ parDefault (flip cont) v2 v1 (returnS $ cont v1 v2)
 where
  cont x y = func f [x, y]

peLitOrder :: QName -> Literal -> Literal -> Expr
peLitOrder f l1 l2
  | f == prelLt  = mkBool (l1 <  l2)
  | f == prelGt  = mkBool (l1 >  l2)
  | f == prelLeq = mkBool (l1 <= l2)
  | f == prelGeq = mkBool (l1 >= l2)
  | otherwise    = error $ "PeNatural.peLitOrder: " ++ show f

peConsOrder :: QName -> [QName] -> QName -> QName -> [Expr] -> [Expr] -> Expr
peConsOrder f cs c1 c2 es1 es2
  | f == prelLt = case ordering of
      LT -> mkBool True
      EQ -> mkOrderCall prelLt prelLt  (zip es1 es2)
      GT -> mkBool False
  | f == prelLeq = case ordering of
      LT -> mkBool True
      EQ -> mkOrderCall prelLt prelLeq (zip es1 es2)
      GT -> mkBool False
  | f == prelGt  = case ordering of
      LT -> mkBool False
      EQ -> mkOrderCall prelGt prelGt  (zip es1 es2)
      GT -> mkBool True
  | f == prelGeq = case ordering of
      LT -> mkBool False
      EQ -> mkOrderCall prelGt prelGeq (zip es1 es2)
      GT -> mkBool True
  | otherwise    = error $ "PeNatural.peConsOrder: " ++ show f
  where
    ordering       = compare (sureIndex c1 cs) (sureIndex c2 cs)
    sureIndex x xs = fromMaybe (error "PeNatural.sureIndex") (elemIndex x xs)

mkOrderCall :: QName -> QName -> [(Expr, Expr)] -> Expr
mkOrderCall pre fin pairs = case pairs of
  []             -> mkBool True
  [(x, y)]       -> func fin [x, y]
  ((x, y) : xys) -> func prelOr [ func pre [x, y]
                                , func prelAnd  [ func prelEq [x, y]
                                                , mkOrderCall pre fin xys
                                                ]
                                ]

--- Partial evaluation of strict unification.
peBuiltinUni :: QName -> Expr -> Expr -> PEM Expr
peBuiltinUni f e1 e2 = peval e1 >+= \v1 -> peval e2 >+= \v2 -> case (v1, v2) of
  (Var x, Var y)
    | x == y    -> getBinding x >+= \bdg -> case bdg of
      FreeVar   -> succeedS
      LazyFree  -> succeedS
      _         -> returnS (cont v1 v2)
    | otherwise -> getBinding x >+= \bdg -> case bdg of
      FreeVar  -> uniFree
      LazyFree -> uniFree
      _        -> returnS (cont v1 v2)
    where uniFree = bindE x v2 >+ getBinding y >+= \bdgY -> case bdgY of
            FreeVar  -> succeedS
            LazyFree -> succeedS
            _        -> returnS (cont v1 v2)
  (Var x, Lit l) -> getBinding x >+= \bdg -> case bdg of
    FreeVar  -> bindE x v2 >+ succeedS
    LazyFree -> bindE x v2 >+ succeedS
    _        -> returnS $ Case Flex v1 [Branch (LPattern l) trueExpr]
  (Lit l, Var y) -> getBinding y >+= \bdg -> case bdg of
    FreeVar  -> bindE y v1 >+ succeedS
    LazyFree -> bindE y v1 >+ succeedS
    _        -> returnS $ Case Flex v2 [Branch (LPattern l) trueExpr]
  (Lit l, Lit m)
    | l == m    -> succeedS
    | otherwise -> failS
  (Var x, Comb ConsCall c xs) -> occurCheck x v2
                               $ getBinding x >+= \bdg -> case bdg of
      FreeVar  -> uniFree
      LazyFree -> uniFree
      _        -> mapS (\_ -> freshVar) xs >+= \ys ->
                  pevalOrDefer f (Case Flex v1 [Branch (Pattern c ys)
                    (combine f prelAmp trueExpr (map Var ys) xs)])
    where uniFree = mapS (\_ -> bindFree) xs     >+= \ys ->
                    bindE x (Comb ConsCall c ys) >+
                    pevalOrDefer f (combine f prelAmp trueExpr ys xs)
  (Comb ConsCall c xs, Var y) -> occurCheck y v1
                               $ getBinding y >+= \bdg -> case bdg of
      FreeVar  -> uniFree
      LazyFree -> uniFree
      _        -> mapS (\_ -> freshVar) xs >+= \ys ->
                  pevalOrDefer f (Case Flex v2 [Branch (Pattern c ys)
                    (combine f prelAmp trueExpr xs (map Var ys))])
    where uniFree = mapS (\_ -> bindFree) xs >+= \ys ->
                    bindE y (Comb ConsCall c ys) >+
                    pevalOrDefer f (combine f prelAmp trueExpr xs ys)
  (Comb ConsCall c1 es1, Comb ConsCall c2 es2)
    | c1 == c2  -> pevalOrDefer f $ combine f prelAmp trueExpr es1 es2
    | otherwise -> failS
  _ | all (== trueExpr) [v1, v2] -> succeedS
    | otherwise            -> parDefault cont v1 v2
                            $ parDefault (flip cont) v2 v1
                              (returnS $ cont v1 v2)
 where cont x y = func f [x, y]

--- Occur check for strict unification.
occurCheck :: VarIndex -> Expr -> PEM Expr -> PEM Expr
occurCheck v e act = getHeap >+= \h ->
                     if v `elem` critVars h e then failS else act

--- Determine the set of critical variables for a heap and an expression.
critVars :: Heap -> Expr -> [VarIndex]
critVars h (Var        x) = case H.lookupHeap x h of
  Just (BoundVar  e) -> critVars (H.unbind x h) e
  Just (LazyBound e) -> critVars (H.unbind x h) e
  _                  -> [x]
critVars _ (Lit        _) = []
critVars h c@(Comb ct _ es) = case getSQ c of
  Just e -> critVars h e
  _      -> case ct of
    ConsCall -> nub $ concatMap (critVars h) es
    _        -> []
critVars h (Let     ds e) = critVars h e \\ map fst ds
critVars h (Free    vs e) = critVars h e \\ vs
critVars h (Or     e1 e2) = critVars h e1 `intersect` critVars h e2
critVars h (Case  _ _ bs) = foldr1 intersect (map critBranch bs)
  where critBranch (Branch p be) = critVars h be \\ patVars p
critVars h (Typed    e _) = critVars h e

--- Partial evaluation of lazy unification (functional patterns).
peBuiltinLazyUni :: QName -> Expr -> Expr -> PEM Expr
peBuiltinLazyUni f e1 e2 = peval e1 >+= \v1 -> case v1 of
  Var x              -> getBinding x >+= \bdg -> case bdg of
    FreeVar   -> bindLE x e2 >+ succeedS
    LazyFree  -> bindF  x    >+ peval (v1 .=:=. e2)
    LazyParam -> bindP  x    >+ peval (v1 .=:=. e2)
    _         -> returnS $ cont v1 e2
  Lit l              -> peval e2 >+= \v2 -> case v2 of
    Lit m
      | l == m    -> succeedS
      | otherwise -> failS
    Var y         -> getBinding y >+= \bdg -> case bdg of
      Param       -> returnS $ Case Flex v2 [Branch (LPattern l) trueExpr]
      FreeVar     -> bindE y v1 >+ succeedS
      _           -> parDefault (flip cont) v2 v1 (returnS $ cont v1 v2)
    _             -> parDefault (flip cont) v2 v1 (returnS $ cont v1 v2)
  Comb ConsCall c xs -> peval e2 >+= \v2 -> case v2 of
    Comb ConsCall d ys
      | c == d     -> pevalOrDefer f $ combine f prelAmp trueExpr xs ys
      | otherwise  -> failS
    Var y -> getBinding y >+= \bdg -> case bdg of
      Param    -> mapS (\_ -> freshVar) xs >+= \ys ->
                  pevalOrDefer f (Case Flex v2 [Branch (Pattern c ys)
                  (combine f prelAmp trueExpr xs (map Var ys))])
      LazyParam-> mapS (\_ -> freshVar) xs >+= \ys ->
                  pevalOrDefer f (Case Flex v2 [Branch (Pattern c ys)
                  (combine f prelAmp trueExpr xs (map Var ys))])
      LazyFree -> mapS (\_ -> bindFree) xs >+= \ys ->
                  bindE y (Comb ConsCall c ys) >+
                  pevalOrDefer f (combine f prelAmp trueExpr xs ys)
      FreeVar  -> mapS (\_ -> bindFree) xs >+= \ys ->
                  bindE y (Comb ConsCall c ys) >+
                  pevalOrDefer f (combine f prelAmp trueExpr xs ys)
      _        -> parDefault (flip cont) v2 v1 (returnS $ cont v1 v2)
    _  -> parDefault cont v1 v2 $ returnS (cont v1 v2)
  _  -> parDefault cont v1 e2 $ returnS (cont v1 e2)
 where cont x y = func f [x, y]

--- Default behaviour for parallel binary operations.
--- This function is called to handle those cases where the builtin function
--- is unable to make some progress.
parDefault :: (Expr -> Expr -> Expr) -> Expr -> Expr -> PEM Expr -> PEM Expr
parDefault cont e1 e2 alt = case getSQ e1 of
  -- Progress may be possible later -> defer
  Just e' -> defer (cont e' e2)
  _       -> case e1 of
    -- Lift case expression to add more information.
    Case ct e' bs        -> mapS freshBranch bs >+= \bs' ->
                            peval (Case ct e' (flip cont e2 `onBranchExps` bs'))
    -- Propagate Failure
    _ | e1 == failedExpr -> failS
    -- otherwise try alternative
      | otherwise        -> alt

--- Default behaviour for sequential (left-to-right) binary operations.
--- This function is called to handle those cases where the builtin function
--- is unable to make some progress.
seqDefault :: (Expr -> Expr -> Expr) -> Expr -> Expr -> PEM Expr -> PEM Expr
seqDefault cont e1 e2 alt = case getSQ e1 of
  -- Progress may be possible later -> defer
  Just e' -> defer (cont e' e2)
  _       -> case e1 of
    Var x -> getBinding x >+= \bdg -> case bdg of
      -- No information -> directly continue with e2
      Param     -> cont e1 <$> defer e2
      LazyParam -> cont e1 <$> defer e2
      _         -> alt
    -- residual case expression -> lift
    Case ct e' bs        -> mapS freshBranch bs >+= \bs' ->
                            peval (Case ct e' (flip cont e2 `onBranchExps` bs'))
    Comb FuncCall _ _
      -- failure -> propagate
      | e1 == failedExpr -> failS
      -- residual primitive -> lift
      | otherwise        -> cont e1 <$> peval e2
    -- otherwise: try alternative
    _                    -> alt
