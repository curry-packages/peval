--- ----------------------------------------------------------------------------
--- This module performs some compression on the resultants.
--- This is necessary because the termination criteria leads to many
--- intermediate specializations which occur only once
--- and can therefore be inlined.
---
--- Note that some optimizations may never be applied depending on the partial
--- evaluation mechanism.
---
--- @author Elvira Albert, German Vidal, Michael Hanus, Björn Peemöller
--- @version September 2015
--- ----------------------------------------------------------------------------
module PostUnfold (unAlias, postUnfold, removeCopies) where

import System.Console.ANSI.Codes (yellow)
import Data.Tuple.Extra          (first, second)
import Data.List                 (delete, find, partition)

import Text.Pretty      (Doc, (<+>), ($$), pPrint, text)
import FlatCurry.Types
import FlatCurryGoodies ( addPartCallArg, funcsInExps, isFuncCall, isPartCall
                        , isVar, maximumVarIndex, mkLet, trExpr
                        , prelApply, prelFailed, prelPEVAL )
import FlatCurryPretty  (ppExp, indent)
import Utils            (count, countBy)
import NameChange       (NameChange, ncRenaming, ncResultants, ncExpr)
import Normalization    ( simplifyExpr, normalizeFreeExpr
                        , freshResultant, renameResultant)
import Output           (colorWith, traceDetail)
import PevalBase        (Renaming, Resultant, ppResultant, mkFuncCall)
import PevalOpts        (Options)

-- -----------------------------------------------------------------------------
-- Interface
-- -----------------------------------------------------------------------------

--- Compress the resultants.
--- @param fs - List of functions called by user-annotated expressions.
--- @param rs - Resultants to be compressed.
postUnfold :: Options -> [QName] -> Renaming -> [Resultant]
           -> (Renaming, [Resultant])
postUnfold opts fs rho0 rs0 = go rho0 rs0
 where
  go rho rs | rs' == rs = (rho, rs)
            | otherwise = go rho' rs'
    where (rho', rs') = unAlias rho (puResultants opts fs rs)

--- Remove aliases and duplicates.
unAlias :: Renaming -> [Resultant] -> (Renaming, [Resultant])
unAlias rho rs = remove duplicates $ remove aliases (rho, rs)

--- Removes any resultants that are copies of ordinary program functions
--- to avoid code duplication.
removeCopies :: Prog -> Renaming -> [Resultant] -> (Renaming, [Resultant])
removeCopies (Prog _ _ _ fs _) rho rs = remove (copies fs rho) (rho, rs)

-- -----------------------------------------------------------------------------
-- Removal of duplicates and alike
-- -----------------------------------------------------------------------------

--- Identify aliases in the list of resultants such as
--- `f x1 ... xn = g x1 ... xn`.
-- TODO: This implementation only replaces fully applied function calls,
-- thus in the following example no progress is made:
-- f   = g
-- g x = x
-- because `g` is a partial function call. This could be remedied by not only
-- covering *name* changes, but also *arity* changes, i.e., one could replace
-- `Comb FuncCall f []` by `Comb (FuncPartCall 1) g []`.
aliases :: [Resultant] -> (NameChange, [Resultant])
aliases = foldr checkAlias ([], [])
  where
  checkAlias r@((f, vs), e) (as, rs) = case e of
    Comb FuncCall g es | f /= g && es == map Var vs
      -> ((f, g) : as,     rs)
    _ -> (         as, r : rs)

--- Identify duplicates in the list of resultants such as
--- `f x1 ... xn = e` and g x1 ... xn = e'` where `e' = e[f/g]`.
duplicates :: [Resultant] -> (NameChange, [Resultant])
duplicates []                  = ([], [])
duplicates (r@((f, vs), e) : rs) = case break isDuplicate rs of
  (_  , []             ) -> second (r:) (duplicates rs)
  (rs1, ((g, _), _):rs2) -> let (als, rs') = duplicates (rs1 ++ rs2)
                            in  ((g, f) : als, r : rs')
 where isDuplicate ((g, vs'), e') = vs == vs' && ncExpr [(f, g)] e == e'

--- Identify copies of program functions in the list of resultants such as
--- `f x1 ... xn = e` where g x1 ... xn = e` is contained in the program.
copies :: [FuncDecl] -> Renaming -> [Resultant] -> (NameChange, [Resultant])
copies _  _   []                    = ([], [])
copies fs rho (r@((f, vs), e) : rs) = case find isCopy fs of
  Nothing               -> second (r:)       (copies fs rho rs)
  Just (Func g _ _ _ _) -> first  ((f, g) :) (copies fs rho rs)
 where
  isCopy (Func g _ _ _ rule) = case rule of
    Rule vs' e' -> vs == vs' && ncExpr [(f, g)] e == e'
                   && (mkFuncCall (g, map negate vs), (f, map negate vs))
                      `elem` rho
    _           -> False

--- Remove resultants that are either aliases for or duplicates of each other.
remove :: ([Resultant] -> (NameChange, [Resultant]))
       -> (Renaming, [Resultant]) -> (Renaming, [Resultant])
remove identify (rho, rs) =  (ncRenaming nc rho, ncResultants nc rs')
  where
  (as, rs')        = identify rs
  nc               = map (\(f, g) -> (f, deepLookup g as)) as
  deepLookup x xys = case lookup x xys of
    Nothing -> x
    Just y  -> deepLookup y xys

-- -----------------------------------------------------------------------------
-- Post-Unfolding
-- -----------------------------------------------------------------------------

--- Post-Unfold a list of resultants.
puResultants :: Options -> [QName] -> [Resultant] -> [Resultant]
puResultants opts fs rs = removeRedundantRules fs
                        $ map (puResultant (initEnv opts rs)) rs

--- Remove resultants which are not used in any of the right-hand-sides
--- of other resultants.
removeRedundantRules :: [QName] -> [Resultant] -> [Resultant]
removeRedundantRules fs rs = filter (not . redundant) rs
  where redundant r@((f, _), _) = f `notElem` fs && countFuncCalls f
                                             (map snd (delete r rs)) == 0

--- The post-unfolding environment.
data PUEnv = PUEnv
  { peOptions    :: Options
  , peResultants :: [Resultant]
  }

--- Create the initial post-unfolding environment.
initEnv :: Options -> [Resultant] -> PUEnv
initEnv opts rs = PUEnv opts rs

--- Post-Unfold a resultant.
puResultant :: PUEnv -> Resultant -> Resultant
puResultant env = renameResultant . second (puExpr env)

--- Trace a post-unfolding action.
puTrace :: PUEnv -> Doc -> a -> a
puTrace PUEnv { peOptions = o } doc x = traceDetail o (colorWith o yellow str) x
  where str = pPrint (text "Compression:" <+> doc) ++ "\n"

--- Post-Unfold an expression.
puExpr :: PUEnv -> Expr -> Expr
puExpr env = normalizeFreeExpr
           . simplifyExpr
           . trExpr Var Lit (puComb env) Free Or Case Branch Let Typed

--- Compress higher-order application and simple calls.
puComb :: PUEnv -> CombType -> QName -> [Expr] -> Expr
puComb env ct qn es = case ct of
  FuncCall | qn == prelApply -> puApply es
           | qn == prelPEVAL -> Comb ct qn es
           | otherwise       -> puFuncCall env qn es
  _                          -> Comb ct qn es

--- Apply a part call to another expression.
puApply :: [Expr] -> Expr
puApply exs = case exs of
  [Comb ct f es, e2] | isPartCall ct -> addPartCallArg ct f es e2
  [_, _]                             -> Comb FuncCall prelApply exs
  _                                  -> error "PostUnfold.puApply"

--- Unfolding of a function call.
puFuncCall :: PUEnv -> QName -> [Expr] -> Expr
puFuncCall env f es = case find ((== f) . resultantName) (peResultants env) of
  Just r | isSimple r || isAlias r || isIntermediate env r
        -> unfold env r es
  _     -> Comb FuncCall f es

--- Retrieve the name of a resultant.
resultantName :: Resultant -> QName
resultantName ((f, _), _) = f

--- A simple function does not call any other function.
isSimple :: Resultant -> Bool
isSimple ((_, _), e) = countFuncs [e] == 0

--- An alias function simply calls another function with a possibly changed
--- variable order.
isAlias :: Resultant -> Bool
isAlias ((_, _), e) = case e of
  Comb FuncCall _ es -> all isVar es
  _                  -> False

--- An intermediate function is a function which is only called once
--- and is not recursive.
isIntermediate :: PUEnv -> Resultant -> Bool
isIntermediate env ((f, _), e)
  =  countFuncCalls f [e] == 0
  && countFuncCalls f (map snd (peResultants env)) == 1

--- Unfold a function call by introducing a let binding for arguments.
unfold :: PUEnv -> Resultant -> [Expr] -> Expr
unfold opts r es = puTrace opts doc e''
  where
  r'@((f, vs'), e') = freshResultant (maximumVarIndex es + 1) r
  e''               = mkLet (zip vs' es) e'
  doc =  indent (text "Unfolding call"  $$ ppExp (Comb FuncCall f es))
      $$ indent (text "with definition" $$ ppResultant r')
      $$ indent (text "to expression"   $$ ppExp e'')

--- Count the number of calls to any function
--- (with exception of failed which cannot be reduced).
countFuncs :: [Expr] -> Int
countFuncs = countBy (`notElem` [prelFailed]) . funcsInExps

--- Count the number of calls to a specific function.
countFuncCalls :: QName -> [Expr] -> Int
countFuncCalls f = count f . funcsInExps
