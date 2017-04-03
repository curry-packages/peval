--- ----------------------------------------------------------------------------
--- This module defines utility functions for working with FlatCurry.
---
--- @author Björn Peemöller
--- @version April 2015
--- ----------------------------------------------------------------------------
module FlatCurryGoodies where

import Function        (second)
import List            (nub, union, maximum)
import Maybe           (isJust)
import Utils           (disjoint)

import FlatCurry.Types

-- ---------------------------------------------------------------------------
-- Functions on programs and function/type declarations
-- ---------------------------------------------------------------------------

--- Retrieve the module name of a program.
progName :: Prog -> String
progName (Prog m _ _ _ _) = m

--- Retrieve the function's name from its declaration.
funcName :: FuncDecl -> QName
funcName (Func f _ _ _ _) = f

--- Is a function externally defined?
isExternal :: FuncDecl -> Bool
isExternal (Func _ _ _ _ r) = case r of
  External _ -> True
  _          -> False

--- Has a function declaration the given name?
hasName :: QName -> FuncDecl -> Bool
hasName qn f = funcName f == qn

--- Retrieve the constructors of a type.
constructors :: TypeDecl -> [QName]
constructors (Type    _ _ _ cs) = [ c | Cons c _ _ _ <- cs ]
constructors (TypeSyn _ _ _  _) = []

-- ---------------------------------------------------------------------------
-- Functions on expressions
-- ---------------------------------------------------------------------------

--- Get the maximum index of all given variables of 0 if the list is empty.
maxVar :: [VarIndex] -> VarIndex
maxVar vs = maximum (0 : vs)

--- Get the maximum index of all variables in an expression,
--- or 0 if there is no variable at all.
maxVarIndex :: Expr -> VarIndex
maxVarIndex e = maximumVarIndex [e]

--- Get the maximum index of all variables in a list of expressions,
--- or 0 if there is no variable at all.
maximumVarIndex :: [Expr] -> VarIndex
maximumVarIndex es = maxVar (concatMap vars es)

--- Get all variables of an expression, without duplicates.
vars :: Expr -> [VarIndex]
vars (Var        v) = [v]
vars (Lit        _) = []
vars (Comb  _ _ es) = foldr union [] (map vars es)
vars (Free    vs e) = vs `union` vars e
vars (Or     e1 e2) = vars e1 `union` vars e2
vars (Case  _ e bs) = foldr union (vars e) (map inBranch bs)
  where inBranch (Branch p be) = vars be `union` nub (patVars p)
vars (Let     bs e) = foldr union vs (map vars (e : es))
  where (vs, es) = unzip bs
vars (Typed    e _) = vars e

--- Get all free variables of an expression, without duplicates.
freeVars :: Expr -> [VarIndex]
freeVars = nub . freeVarsDup

--- Get all free variables of an expression, with duplicates.
freeVarsDup :: Expr -> [VarIndex]
freeVarsDup (Var        v) = [v]
freeVarsDup (Lit        _) = []
freeVarsDup (Comb  _ _ es) = concatMap freeVarsDup es
freeVarsDup (Free    vs e) = freeVarsDup e \\\ vs
freeVarsDup (Or     e1 e2) = freeVarsDup e1 ++ freeVarsDup e2
freeVarsDup (Case  _ e bs) = concat (freeVarsDup e : map inBranch bs)
  where inBranch (Branch p be) = freeVarsDup be \\\ patVars p
freeVarsDup (Let     bs e) = concatMap freeVarsDup (e : es) \\\ vs
  where (vs, es) = unzip bs
freeVarsDup (Typed    e _) = freeVarsDup e

--- List difference with duplicates.
(\\\) :: [a] -> [a] -> [a]
xs \\\ ys = filter (`notElem` nub ys) xs

--- Get all used functions in a list of expression, with duplicates.
funcsInExps :: [Expr] -> [QName]
funcsInExps = concatMap funcsInExp
  where
  funcsInExp (Var        _) = []
  funcsInExp (Lit        _) = []
  funcsInExp (Comb  t f es) = case t of
    FuncCall       -> [f | f /= prelPEVAL] ++ concatMap funcsInExp es
    FuncPartCall _ -> [f | f /= prelPEVAL] ++ concatMap funcsInExp es
    _              -> concatMap funcsInExp es
  funcsInExp (Free     _ e) = funcsInExp e
  funcsInExp (Or     e1 e2) = funcsInExp e1 ++ funcsInExp e2
  funcsInExp (Case  _ e cs) = concatMap funcsInExp (e : branchExprs cs)
  funcsInExp (Let     bs e) = concatMap (funcsInExp . snd) bs ++ funcsInExp e
  funcsInExp (Typed    e _) = funcsInExp e

--- Check that no shadowing of variables occurs in the given expression.
noShadowing :: Expr -> Bool
noShadowing = cns []
  where
  cns vs ex = case ex of
    Var _       -> True
    Lit  _      -> True
    Comb _ _ es -> all (cns vs) es
    Free xs e   -> disjoint vs xs && cns (vs ++ xs) e
    Or e1 e2    -> cns vs e1 && cns vs e2
    Case _ e bs -> cns vs e && all cnsBranch bs
      where cnsBranch (Branch p be) = let xs = patVars p
                                      in  disjoint vs xs && cns (vs ++ xs) be
    Let ds e    -> let (xs, es) = unzip ds
                   in  disjoint vs xs && all (cns (vs ++ xs)) (e:es)
    Typed e _   -> cns vs e

--- Compute all sub-expressions of an expression.
subExprs :: Expr -> [Expr]
subExprs e = case e of
  Var   _       -> [e]
  Lit   _       -> [e]
  Comb  _ _ es  -> e : concatMap subExprs es
  Let   ds e'   -> e : concatMap subExprs (e' : map snd ds)
  Free  _ e'    -> e  : subExprs e'
  Or    e1 e2   -> e : subExprs e1 ++ subExprs e2
  Case  _ e' bs -> e : concatMap subExprs (e' : branchExprs bs)
  Typed e' _    -> e : subExprs e'

-- --------------------------------------------------------------------------
-- Square brackets, marking expressions for further evaluation
-- --------------------------------------------------------------------------

isSQ :: Expr -> Bool
isSQ e = isJust (getSQ e)

getSQ :: Expr -> Maybe Expr
getSQ e = case e of
  Comb ct f [e'] | ct == FuncCall && f == prelPEVAL -> Just e'
  _                                                 -> Nothing

--- Deeply delete all square brackets from an expression.
delSQ :: Expr -> Expr
delSQ = trExpr Var Lit comb Free Or Case Branch Let Typed
  where
  comb ct f es = let call = Comb ct f es in case getSQ call of
                  Just e  -> e
                  Nothing -> call

--- Create a square bracket at top-level and delete all other square brackets.
topSQ :: Expr -> Expr
topSQ e = Comb FuncCall prelPEVAL [delSQ e]

--- Lift any nested SQ's to top-level or leave the expression as is if there
--- are none.
liftSQ :: Expr -> Expr
liftSQ e = if hasSQ e then topSQ e else e

--- We could not compute an appropriate generalization. To ensure termination,
--- we discard one piece of information and try again.
sq' :: Expr -> Expr
sq' x = case x of
  Free vs e     -> Free vs (sq e)
  Let  ds e     -> Let (map (second sq) ds) (sq e)
  Case ct e bs  -> Case ct (sq e) (sq `onBranchExps` bs)
  Comb ct qn es -> Comb ct qn (map sq es)
  Or e1 e2      -> Or (sq e1) (sq e2)
  _             -> sq x

--- Mark an expression for later evaluation when appropriate.
sq :: Expr -> Expr
sq e = case e of
  Var _                         -> e
  Lit _                         -> e
  Comb ct c es | isConsCall ct  -> Comb ct c (map sq es)
--   Or e1 e2                      -> Or (sq e1) (sq e2)
  Case ct (Var x) bs            -> Case ct (Var x) (sq `onBranchExps` bs)
  Typed e' ty                   -> Typed (sq e') ty
  _ | isFailed e         -> e
    | otherwise                 -> topSQ e

--- Check whether the expression has a square bracket inside.
hasSQ :: Expr -> Bool
hasSQ = trExpr (\_ -> False) (\_ -> False) comb (\_ b -> b)
               (||) (\_ b bs -> or (b:bs)) (\_ b -> b)
               (\ds b -> or (b : map snd ds)) (\b _ -> b)
  where comb ct f bs = or ((ct == FuncCall && f == prelPEVAL) : bs)

-- --------------------------------------------------------------------------
-- Generic functions
-- --------------------------------------------------------------------------

--- Apply a function on all branches' expressions.
onBranchExps :: (Expr -> Expr) -> [BranchExpr] -> [BranchExpr]
onBranchExps f = map (\ (Branch p e) -> Branch p (f e))

--- Unzip a list of branches into a pair of pattern and expression list.
unzipBranches :: [BranchExpr] -> ([Pattern], [Expr])
unzipBranches []                = ([], [])
unzipBranches (Branch p e : bs) = let (ps, es) = unzipBranches bs
                                  in  (p : ps, e : es)

--- Zip a list of patterns and a list of expressions to a list of branches.
zipBranches ::[Pattern] -> [Expr] -> [BranchExpr]
zipBranches = zipWith Branch

--- Retrieve a branches' expression.
branchExpr :: BranchExpr -> Expr
branchExpr (Branch _ e) = e

--- Retrieve all branches' expressions.
branchExprs :: [BranchExpr] -> [Expr]
branchExprs = map branchExpr

--- Retrieve all branches' patterns.
branchPats :: [BranchExpr] -> [Pattern]
branchPats bs = map (\ (Branch p _) -> p) bs

--- Check if two pattern are equal.
eqPattern :: Pattern -> Pattern -> Bool
eqPattern p1 p2 = case (p1, p2) of
  (Pattern f _, Pattern g _) -> f == g
  (LPattern  l, LPattern  m) -> l == m
  _                          -> False

--- Do two lists of case branches have the same patterns?
samePattern :: [BranchExpr] -> [BranchExpr] -> Bool
samePattern bs1 bs2 = length bs1 == length bs2
  && and (zipWith eqPattern (branchPats bs1) (branchPats bs2))

--- Find the matching branch for a given pattern.
findBranch :: Pattern -> [BranchExpr] -> Maybe ([VarIndex], Expr)
findBranch _ []                                = Nothing
findBranch p (Branch q e : bs) | eqPattern p q = Just (patVars q, e)
                               | otherwise     = findBranch p bs

--- Bottom up transformation on expressions.
trExpr :: (VarIndex -> a)
       -> (Literal -> a)
       -> (CombType -> QName -> [a] -> a)
       -> ([VarIndex] -> a -> a)
       -> (a -> a -> a)
       -> (CaseType -> a -> [b] -> a)
       -> (Pattern -> a -> b)
       -> ([(VarIndex, a)] -> a -> a)
       -> (a -> TypeExpr -> a)
       -> Expr -> a
trExpr fVar fLit fComb fFree fOr fCase fBranch fLet fTy x
  = case x of
    Var v        -> fVar v
    Lit l        -> fLit l
    Comb ct c es -> fComb ct c (map f es)
    Free vs e    -> fFree vs (f e)
    Or e1 e2     -> fOr (f e1) (f e2)
    Case ct e bs -> fCase ct (f e) (map (\(Branch p be) -> fBranch p (f be)) bs)
    Let bs e     -> fLet (map (second f) bs) (f e)
    Typed e ty   -> fTy (f e) ty
  where f = trExpr fVar fLit fComb fFree fOr fCase fBranch fLet fTy

-- ---------------------------------------------------------------------------
-- Predicates and selectors on expressions
-- ---------------------------------------------------------------------------

--- Check whether an expression is a variable
isVar :: Expr -> Bool
isVar e = case e of
  Var _ -> True
  _     -> False

--- Check whether an expression is a literal
isLit :: Expr -> Bool
isLit e = case e of
  Lit _ -> True
  _     -> False

--- Check whether a combination is a (partial) constructor call.
isConsCall :: CombType -> Bool
isConsCall c = case c of
  ConsCall       -> True
  ConsPartCall _ -> True
  _              -> False

--- Check whether a combination is a (partial) function call.
isFuncCall :: CombType -> Bool
isFuncCall c = case c of
  FuncCall       -> True
  FuncPartCall _ -> True
  _              -> False

--- Check whether an expression is a partial call.
isPartCall :: CombType -> Bool
isPartCall c = case c of
  ConsPartCall _ -> True
  FuncPartCall _ -> True
  _              -> False

--- Check whether an expression is a constructor term,
--- i.e, it consists of variables, literals and constructors only.
isConstrTerm :: Expr -> Bool
isConstrTerm e = case e of
  Var _                      -> True
  Lit _                      -> True
  Comb ConsCall         _ es -> all isConstrTerm es
  Comb (ConsPartCall _) _ es -> all isConstrTerm es
  Comb (FuncPartCall _) _ es -> all isConstrTerm es
  Comb FuncCall         _ _  -> case getSQ e of
                                  Just e' -> isConstrTerm e'
                                  _       -> isFailed e
  Typed e' _                 -> isConstrTerm e'
  _                          -> False

--- Extract all variables bound by a pattern.
patVars :: Pattern -> [VarIndex]
patVars (Pattern _ vs) = vs
patVars (LPattern   _) = []

-- ---------------------------------------------------------------------------
-- Completion of partial calls
-- ---------------------------------------------------------------------------

--- Add an argument to a PartCall, resulting in either a FuncCall or a PartCall.
addPartCallArg :: CombType -> QName -> [Expr] -> Expr -> Expr
addPartCallArg ct f es e = Comb ct' f (es ++ [e])
  where ct' = case ct of
                ConsPartCall 1 -> ConsCall
                ConsPartCall n -> ConsPartCall (n - 1)
                FuncPartCall 1 -> FuncCall
                FuncPartCall n -> FuncPartCall (n - 1)
                _              -> error "FlatCurryGoodies.addPartCallArg"

--- Completes the arguments of a partcall with fresh variables.
--- /Note:/ There is no guarantee that the new variables are globally fresh,
--- so they must not be used afterwards.
completePartCall :: CombType -> QName -> [Expr] -> Expr
completePartCall ct f es = case ct of
  ConsPartCall m -> Comb ConsCall f (es ++ freshVars m)
  FuncPartCall m -> Comb FuncCall f (es ++ freshVars m)
  _              -> error "FlatCurryGoodies.completePartCall"
 where freshVars n = map Var $ take n [maximumVarIndex es + 1 ..]

--- Compute the number of missing argument for a function or constructor call.
missingArgs :: CombType -> Int
missingArgs c = case c of
  ConsPartCall m -> m
  FuncPartCall m -> m
  _              -> 0

-- ---------------------------------------------------------------------------
-- Constructing new expressions.
-- ---------------------------------------------------------------------------

--- Create an expression from a pattern.
pat2exp :: Pattern -> Expr
pat2exp (Pattern c vs) = cons c (map Var vs)
pat2exp (LPattern   l) = Lit l

--- Smart constructor for `Free` expression, returns the expression itself
--- when no variables are given.
mkFree :: [VarIndex] -> Expr -> Expr
mkFree vs e | null vs   = e
            | otherwise = Free vs e

--- Smart constructor for `Let` expression, returns the expression itself
--- when no declarations are given.
mkLet :: [(VarIndex, Expr)] -> Expr -> Expr
mkLet ds e | null ds   = e
           | otherwise = Let ds e

--- Smart constructor for `Or` expression, returns one of the argument
--- sexpressions if the other one equals `failed`.
mkOr :: Expr -> Expr -> Expr
mkOr e1 e2 | e1 == failedExpr = e2
           | e2 == failedExpr = e1
           | otherwise        = Or e1 e2

--- Smart constructor for `Case` expression, removes branches directly
--- calling failed and evaluates to `failed` if there are only failing branches.
mkCase :: CaseType -> Expr -> [BranchExpr] -> Expr
mkCase ct e bs | null bs'  = failedExpr
               | otherwise = Case ct e bs'
  where
  bs' = filter (not . isFailedBranch) bs
  isFailedBranch (Branch _ be) = be == failedExpr

--- Add a list of lazy bindings to an expression.
mkLazyBind :: [(VarIndex, Expr)] -> Expr -> Expr
mkLazyBind fs e | null fs   = e
                | otherwise = func prelCond [foldr1 comb (map mkUni fs), e]
  where
  mkUni (v, b) = func prelLazyUni [Var v, b]
  comb  a b    = func prelAmp     [a, b]

--- Create a function application.
func :: QName -> [Expr] -> Expr
func = Comb FuncCall

--- Create a constructor application.
cons :: QName -> [Expr] -> Expr
cons = Comb ConsCall

--- Qualify an identifier with the "Prelude" module.
prelude :: String -> QName
prelude x = ("Prelude", x)

--- Is the given expression equal to `failed`?
isFailed :: Expr -> Bool
isFailed e = e == failedExpr

--- Expression representing `True`.
trueExpr :: Expr
trueExpr = cons prelTrue []

--- Expression representing `False`.
falseExpr :: Expr
falseExpr = cons prelFalse []

--- Expression representing `failed`.
failedExpr :: Expr
failedExpr = func prelFailed []

--- `combine inner outer def es1 es2` combines two lists of expressions
--- `es1` and `es2` by combining each pair using `inner`, and then each
--- combination using `outer`. If `es1` or `es2` are empty, `def` is returned.
combine :: QName -> QName -> Expr -> [Expr] -> [Expr] -> Expr
combine eq con def es1 es2
  | null eqs  = def
  | otherwise = foldr1 (mkCall con) eqs
  where
  eqs = zipWith (mkCall eq) es1 es2
  mkCall f e1 e2 = func f [e1, e2]

--- Create a boolean expression based on the specified value.
mkBool :: Bool -> Expr
mkBool True  = trueExpr
mkBool False = falseExpr

--- Combine two expression using strict unification.
(.=:=.) :: Expr -> Expr -> Expr
e1 .=:=. e2 = func prelUni [e1, e2]

--- Combine two expression using the `cond` operator.
(.&>.) :: Expr -> Expr -> Expr
c .&>. e = func prelCond [c, e]

--- `QName` of `Prelude.PEVAL`
prelPEVAL :: QName
prelPEVAL = prelude "PEVAL"

--- `QName` of `Prelude.False`
prelFalse :: QName
prelFalse = prelude "False"

--- `QName` of `Prelude.True`
prelTrue :: QName
prelTrue = prelude "True"

--- `QName` of `Prelude.&>`
prelCond :: QName
prelCond = prelude "&>"

--- `QName` of `Prelude.cond`
prelCond' :: QName
prelCond' = prelude "cond"

--- `QName` of `Prelude.&`
prelAmp :: QName
prelAmp = prelude "&"

--- `QName` of `Prelude.==`
prelEq :: QName
prelEq = prelude "=="

--- `QName` of `Prelude./=`
prelNeq :: QName
prelNeq = prelude "/="

--- `QName` of `Prelude.||`
prelOr :: QName
prelOr = prelude "||"

--- `QName` of `Prelude.&&`
prelAnd :: QName
prelAnd = prelude "&&"

--- `QName` of `Prelude.<`
prelLt :: QName
prelLt = prelude "<"

--- `QName` of `Prelude.<=`
prelLeq :: QName
prelLeq = prelude "<="

--- `QName` of `Prelude.>`
prelGt :: QName
prelGt = prelude ">"

--- `QName` of `Prelude.>=`
prelGeq :: QName
prelGeq = prelude ">="

--- `QName` of `Prelude.success`
prelSuccess :: QName
prelSuccess = prelude "success"

--- `QName` of `Prelude.failed`
prelFailed :: QName
prelFailed = prelude "failed"

--- `QName` of `Prelude.=::=`
prelUni :: QName
prelUni = prelude "=:="

--- `QName` of `Prelude.=:<=`
prelLazyUni :: QName
prelLazyUni = prelude "=:<="

--- `QName` of `Prelude.unknown`
prelUnknown :: QName
prelUnknown = prelude "unknown"

--- `QName` of `Prelude.apply`
prelApply :: QName
prelApply = prelude "apply"

--- `QName` of `Prelude.?`
prelChoice :: QName
prelChoice = prelude "?"

--- `QName` of `Prelude.+`
prelPlus :: QName
prelPlus = prelude "+"

--- `QName` of `Prelude.*`
prelTimes :: QName
prelTimes = prelude "*"

--- `QName` of `Prelude.-`
prelMinus :: QName
prelMinus = prelude "-"

--- `QName` of `Prelude.div`
prelDiv :: QName
prelDiv = prelude "div"

--- `QName` of `Prelude.mod`
prelMod :: QName
prelMod = prelude "mod"
