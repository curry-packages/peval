--- ----------------------------------------------------------------------------
--- This module defines a heap structure, containing bindings from variables
--- to markers for free variables or black holes, or the expressions.
---
--- @author  Björn Peemöller
--- @version May 2015
--- ----------------------------------------------------------------------------
module Heap
  ( Binding (..), ppBinding
  , Heap, ppHeap, emptyHeap, isEmptyHeap, elemHeap, getHeap, lookupHeap
  , bindHole, bindExpr, bindFree, bindParam
  , bindLazyExpr, bindLazyFree, bindLazyParam
  , unbind, dereference, without
  ) where

import Function         (on, second)
import List             ((\\), nub, partition, sortBy)
import Pretty           (Doc, (<>), (<+>), char, listSpaced, text)

import FlatCurry.Types
import FlatCurryGoodies (freeVars, liftSQ, mkFree, mkLazyBind, mkLet, pat2exp
                        , topSQ, patVars, getSQ)
import FlatCurryPretty  (ppExp, ppVarIndex)

--- A 'Binding' represents the value of a variable bound in the heap.
--- @cons BlackHole   - the variable is a black hole, such as in `let x = x in x`
--- @cons BoundVar e  - the variable is bound to an expression `e`
--- @cons FreeVar     - the variable is a logic (free) variable
--- @cons Param     - the variable is an unbound parameter variable
--- @cons LazyBound e - the variable is bound to an expression `e`
--- @cons LazyFree    - the variable is a lazily bound logic (free) variable
--- @cons LazyParam - the variable is a lazily bound parameter variable
data Binding
  = BlackHole
  | BoundVar  Expr
  | LazyBound Expr
  | FreeVar
  | LazyFree
  | Param
  | LazyParam
 deriving Eq

--- Pretty printing of a heap binding
ppBinding :: Binding -> Doc
ppBinding BlackHole     = text "\x25a0"
ppBinding (BoundVar  e) = ppExp e
ppBinding (LazyBound e) = text "~" <> ppExp e
ppBinding FreeVar       = text "free"
ppBinding LazyFree      = text "~free"
ppBinding Param         = text "_"
ppBinding LazyParam     = text "~_"

--- A `Heap` is an association from a `VarIndex` to a binding (see below).
type Heap = [(VarIndex, Binding)]

--- Show the heap in a human readable fashion.
ppHeap :: Heap -> Doc
ppHeap []      = text "[]"
ppHeap h@(_:_) = listSpaced (map ppVarBinding $ sortBy ((>=) `on` fst) h)
  where ppVarBinding (v, b) = ppVarIndex v <+> char '\x21a6' <+> ppBinding b

--- Create an empty heap.
emptyHeap :: Heap
emptyHeap = []

--- Is the given heap empty?
isEmptyHeap :: Heap -> Bool
isEmptyHeap = null

--- Check if a binding exists for a variable in the given heap.
elemHeap :: VarIndex -> Heap -> Bool
elemHeap v h = v `elem` map fst h

--- Get the binding for a variable in the given heap or throw an error
--- in case of a missing binding.
getHeap :: VarIndex -> Heap -> Binding
getHeap v h = case lookup v h of
  Just b  -> b
  Nothing -> Param

--- Lookup a binding for a variable in the given heap.
lookupHeap :: VarIndex -> Heap -> Maybe Binding
lookupHeap = lookup

--- Bind a variable as "baclk hole" to the given expression in the given heap.
bindHole :: VarIndex -> Heap -> Heap
bindHole v = bindHeap v BlackHole

--- Bind a variable to the given expression in the given heap.
bindExpr :: VarIndex -> Expr -> Heap -> Heap
bindExpr v e = bindHeap v (BoundVar e)

--- Bind a variable as "free" in the given heap.
bindFree :: VarIndex -> Heap -> Heap
bindFree v = bindHeap v FreeVar

--- Bind a variable as an unbound parameter in the given heap.
bindParam :: VarIndex -> Heap -> Heap
bindParam v = bindHeap v Param

--- Bind a variable lazily to the given expression in the given heap.
bindLazyExpr :: VarIndex -> Expr -> Heap -> Heap
bindLazyExpr v e = bindHeap v (LazyBound e)

--- Bind a variable lazily as "free" in the given heap.
bindLazyFree :: VarIndex -> Heap -> Heap
bindLazyFree v = bindHeap v LazyFree

--- Bind a variable lazily as an unbound parameter in the given heap.
bindLazyParam :: VarIndex -> Heap -> Heap
bindLazyParam v = bindHeap v LazyParam

--- (internal) Bind a variable to the given binding in the given heap.
bindHeap :: VarIndex -> Binding -> Heap -> Heap
bindHeap v b h = (v, b) : unbind v h

--- Remove any binding for the given variable in the given heap.
unbind :: VarIndex -> Heap -> Heap
unbind v = filter ((/= v) . fst)

--- Remove any bindings for the given variables in the given heap.
unbinds :: [VarIndex] -> Heap -> Heap
unbinds vs = filter ((`notElem` vs) . fst)

-- -----------------------------------------------------------------------------
-- Dereferencing a heap.
-- -----------------------------------------------------------------------------

--- Derefence any bindings in the given heap referred to by the given expression.
--- That is, the bindings of all variables in the expression are extracted
--- from the heap and inserted into the heap as recursive let expressions.
--- This operation preserves sharing by determining the smallest sub-expression
--- which contains all references to a specific variable.
---
--- For example, consider the heap @h = [x -> 1, y -> free]@.
---
--- For this heap, the following equations hold:
--- @
--- dereference h (Var x) == Let [(x, Lit 1)] (Var x)
--- dereference h (Comb FuncCall "+" [Var x, Var x])
---   == Let [(x, Lit 1)] (Comb FuncCall "+" [Var x, Var x])
--- dereference h (Comb FuncCall "+" [Var x, Var y])
---   == Comb FuncCall "+" [Let [(x, Lit 1)] (Var x), Free y (Var y)]
--- @
---
--- /Note:/ Black holes are ignored as they couldn't be pleasantly represented
--- and therefore should be handled elsewhere.
dereference :: Heap -> Expr -> Expr
dereference heap0 expr0 = snd (drf heap0 expr0)
  where

  drf :: Heap -> Expr -> ([VarIndex], Expr)
  drf h expr = case expr of
    Var v         -> ([v], drfVar v h)
    Lit _         -> ([] , expr      )
    Comb ct qn es -> case getSQ expr of
      -- We must use topSQ here because e'' may include SQ's originating
      -- from bindings in the heap.
      Just e -> let (vs, e') = drf h e in (vs, topSQ e')
      _      -> let (shared, h' ) = splitShared h vss
                    (vss   , es') = unzip $ map (drf h') es
                in  (nub (concat vss), addHeap shared (Comb ct qn es'))
    Let ds e      ->
      let (shared, h'      ) = splitShared h vss
          (vs    , es      ) = unzip ds
          (vss   , (e':es')) = unzip $ map (drf (unbinds vs h')) (e:es)
      in  (nub (concat vss) \\ vs, addHeap shared (Let (zip vs es') e'))
    Free vs e     ->
      let (vs', e') = drf (unbinds vs h) e
      in  (vs' \\ vs, Free vs e')
    Or e1 e2      ->
      let (shared, h'        ) = splitShared h vss
          (vss   , [e1', e2']) = unzip $ map (drf h') [e1, e2]
      in  (nub (concat vss), addHeap shared (Or e1' e2'))
    Case ct e bs ->
      let (shared, h'       ) = splitShared h vss'
          (vs    , e'       ) = drf h' e
          (vss   , bs'      ) = unzip $ map (drfBranch e' h') bs
          vss'                = [vs, nub $ concat vss]
      in  (nub (concat vss'), addHeap shared (Case ct e' bs'))
    Typed e ty    -> let (vs, e') = drf h e in (vs, Typed e' ty)

  drfVar v h = case extract h [v] of
    [(_, LazyBound e)] -> e
    h'                 -> addHeap h' (Var v)
    -- addHeap (extract h [v]) (Var v)

  drfBranch :: Expr -> Heap -> BranchExpr -> ([VarIndex], BranchExpr)
  drfBranch v h (Branch p e) = let (vs, e') = drf h' e
                               in (vs \\ patVars p, Branch p e')
    where h' = case v of
                Var x -> bindExpr x (pat2exp p) h
                _     -> h

--- `splitShared h vss = (shared, notShared)` splits a heap `h` into two heaps
--- where the first heap contains the bindings which are reachable from at least
--- two of the variable sets, and the second heap contains the bindings
--- reachable from at most one variable set.
splitShared :: Heap -> [[VarIndex]] -> (Heap, Heap)
splitShared h vss = (sharedHeap, h \\ sharedHeap)
  where
  sharedHeap = extract h shared
  shared     = filter isShared $ nub $ concatMap (map fst) hs
  isShared v = length (filter (elemHeap v) hs) > 1
  hs         = map (extract h) vss

--- Add the given heap as a big let expression to the given expression.
--- If the resulting expression contains a square bracket anywhere, the
--- square bracket will be lifted to top-level.
addHeap :: Heap -> Expr -> Expr
addHeap h e
  | isEmptyHeap h = e
  | otherwise     = liftSQ $ mkFree (getFrees h) $ mkLet (getBound h)
                  $ mkLazyBind (getLazyBound h) e

--- Extract the transitive closure of bindings reachable
--- by the given list of variables.
extract :: Heap -> [VarIndex] -> Heap
extract _ []     = []
extract h (v:vs) = case splitHeap v h of
  (Nothing, _ ) -> extract h vs
  (Just b , h') -> case b of
    BoundVar e  -> (v, b) : extract h' (vs ++ freeVars e)
    LazyBound e -> (v, b) : extract h' (vs ++ freeVars e)
    FreeVar     -> (v, b) : extract h' vs
    LazyFree    -> (v, b) : extract h' vs
    _           ->          extract h' vs

--- Split a heap with a given variable into the variable binding, if any,
--- and the heap without this binding.
splitHeap :: VarIndex -> Heap -> (Maybe Binding, Heap)
splitHeap _ []            = (Nothing, [])
splitHeap v ((w, b) : bs)
  | v == w    = (Just b, bs)
  | otherwise = second ((w, b) :) (splitHeap v bs)

--- Extract all free variables from a heap.
getFrees :: Heap -> [VarIndex]
getFrees h = [ v | (v, FreeVar) <- h ] ++ [ v | (v, LazyBound _) <- h]

--- Extract all expression bindings for variables from a heap.
getBound :: Heap -> [(VarIndex, Expr)]
getBound h = [ (v, e) | (v, BoundVar e) <- h ]

--- Difference on heaps.
without :: Heap -> Heap -> Heap
without h1 h2 = [ b | b@(v, _) <- h1, v `notElem` map fst (getBound h2) ]

--- Extract all expression bindings for variables from a heap.
getLazyBound :: Heap -> [(VarIndex, Expr)]
getLazyBound h = [ (v, e) | (v, LazyBound e) <- h ]
