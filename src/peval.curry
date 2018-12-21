--- ----------------------------------------------------------------------------
--- Curry Partial Evaluator
---
--- Based on the partial evaluator originally developed
--- by Elvira Albert, German Vidal (UPV), and Michael Hanus (CAU Kiel).
---
--- @author  Björn Peemöller and the authors declared above
--- @version December 2018
--- ----------------------------------------------------------------------------
module peval (main) where

import AnsiCodes                 (green)
import FilePath                  ( FilePath, (<.>), dropExtension
                                 , replaceBaseName, takeBaseName, takeDirectory)
import Function                  ((***), first, second)
import List                      ((\\), find, nub)
import Text.Pretty               ( Doc, ($$), (<$+$>), (<+>), char, compose
                                 , pPrint, vsep, text)
import System                    (setEnviron)

import FlatCurry.Annotated.Goodies (unAnnFuncDecl)
import FlatCurry.Annotated.TypeInference (TypeEnv, inferNewFunctions)

import Abstract                  (abstract)
import FlatCurry.Types
import FlatCurry.Files           (readFlatCurry, writeFCY)
import FlatCurryGoodies          (topSQ, sq, freeVars, funcsInExps, onBranchExps)
import FlatCurryPretty           (ppExp, ppFuncDecls, ppProg, indent)
import NameChange                (ncRenaming, ncResultants)
import Normalization             (renameFuncDecl, renameResultant)
import Output                    (colorWith, info, status, detail, traceDetail, traceDebug)
import PeLetRW   as LetRW        (pevalExpr)
import PeNatural as Natural      (pevalExpr)
import PeRLNT    as RLNT         (pevalExpr)
import PevalBase                 (Renaming, Resultant, ppRenaming, ppResultants)
import PevalOpts                 ( Options (..), Semantics (..)
                                 , Abstraction (None), getOpts)
import PostUnfold                (unAlias, postUnfold, removeCopies)
import Renaming                  (renameExpr)

import System.CurryPath          (inCurrySubdir)

-- ---------------------------------------------------------------------------
-- main program
-- ---------------------------------------------------------------------------

-- TODO: Replace inferNewFunctions by getTypeEnv and inferNewFunctionsEnv
-- once it is in the libraries.

--- Main function of the partial evaluator.
main :: IO ()
main = do
  (opts, files) <- getOpts
  mapIO_ (spec opts) files

--- Specialization of a single file.
spec :: Options -> FilePath -> IO ()
spec opts f = do
  -- we set the path to allow imports located
  -- in the directory of the file for type checking
  setEnviron "CURRYPATH" (takeDirectory f)
  readFlatCurry (dropExtension f) >>= specPE opts >>= writeSpec opts f

--- Write the specialised program to the destination file.
writeSpec :: Options -> FilePath -> Prog -> IO ()
writeSpec opts f p = do
  status opts $ "Writing specialized program into file '" ++ dest ++ "'."
  writeFCY dest p
 where dest = specFile opts f

--- Compute the `FilePath` of the specialised program.
specFile :: Options -> FilePath -> FilePath
specFile opts f = inCurrySubdir b' <.> "fcy"
 where b' = replaceBaseName (dropExtension f) (takeBaseName f ++ optSuffix opts)

--- Specialization of a program.
--- Note that during the specialization process, the generated functions are
--- repeatedly type inferred for trace output although the types are later
--- discarded (except for the last run). The reason for this is that some errors
--- in the specialization process manifest in type-incorrect programs so that
--- these errors can be discovered earlier in the process.
specPE :: Options -> Prog -> IO Prog
specPE opts p@(Prog m _ _ _ _) = do
  -- 0. Determine type environment
  -- tyEnv <- getTypeEnv p

  -- 1. Split program into non-annotated program and annotated expressions
  let (prog, es) = extractPeval opts p
  traceStep opts "Annotated Expressions" (vsep . map (indent . ppExp)) es

  -- 2. Partial Evaluation of annotated expressions
  let peResult = pevalProg opts prog es
  showDetail opts "Partial Evaluation Loop" peResult
  traceInfo  opts "Partially Evaluated (Sub)Expressions" ppPevals peResult

  -- 3. Create resultants
  let (ren, res0)     = createResultants m peResult
--       typedRes0 <- createFunctions p res0
  traceInfo opts "Independent Renaming"   ppRenaming   ren
  traceInfo opts "Pre-Partial Evaluation" ppResultants res0

  let res             = renameResultants opts ren res0
  typedResultants <- createFunctions p res
  showDetail opts "Application of Renaming" res
  traceInfo  opts "Partial Evaluation" ppFuncDecls  typedResultants

  -- 4. Remove aliases and duplicates in the list of resultants
  let (ren2, res2) = unAlias ren res
  typedRes2 <- createFunctions p res2
  traceInfo opts "Independent Renaming after Compression" ppRenaming  ren2
  traceInfo opts "Partial Evaluation after Compression"   ppFuncDecls typedRes2

  -- 5. Compress the remaining resultants
  let (ren3, res3) = postUnfolding opts es ren2 res2
  typedRes3 <- createFunctions p res3
  showDetail opts "Compression of Resultants" typedRes3
  traceInfo  opts "Independent Renaming after Inlining" ppRenaming  ren3
  traceInfo  opts "Partial Evaluation after Inlining"   ppFuncDecls typedRes3

  -- 6. Remove any resultants that are copies of program functions
  (ren4, res4) <- if optClosed opts
                    then return (ren3, res3)
                    else do
    let (ren4', res4') = removeCopies prog ren3 res3
    typedRes4 <- createFunctions p res4'
    traceInfo opts "Independent Renaming after Copy Removal" ppRenaming  ren4'
    traceInfo opts "Partial Evaluation after Copy Removal"   ppFuncDecls typedRes4
    return (ren4', res4')

  -- 7. Change names so that resultants are numbered from 0 upwards
  let (ren5, res5) = nameChange m ren4 res4
  typedRes5 <- createFunctions p res5
  traceInfo opts "Final Independent Renaming" ppRenaming  ren5
  traceStep opts "Final Partial Evaluation"   ppFuncDecls typedRes5

  -- 8. Creating the result program
  let p' = integratePeval opts p ren5 typedRes5
  showDetail opts "Renaming in original program" p'
  traceInfo  opts "Resulting program" ppProg p'

  -- Print out resulting program if in debug mode
  when (optDebug opts) $ putStrLn $ pPrint $ ppProg p'

  return p'

--- Pretty-print a list of partial evaluated expressions.
ppPevals :: [(Expr, Expr)] -> Doc
ppPevals = compose (<$+$>) . map ppPeval
  where ppPeval (l, r) = indent (ppExp l) $$ char '\x21d2' <+> indent (ppExp r)

--- Trace a step in the process of evaluation.
traceStep :: Options -> String -> (a -> Doc) -> a -> IO ()
traceStep opts str pp x = showStatus opts str (pPrint . pp $!! x)

--- Trace a step in the process of evaluation.
traceInfo :: Options -> String -> (a -> Doc) -> a -> IO ()
traceInfo opts str pp x = showInfo opts str (pPrint . pp $!! x)

--- Show a status information.
showStatus :: Options -> String -> String -> IO ()
showStatus opts hdr cnt = status opts $
  hdr ++ "\n" ++ replicate (length hdr) '-' ++ "\n" ++ cnt ++ "\n"

--- Show additional information.
showInfo :: Options -> String -> String -> IO ()
showInfo opts hdr cnt = info opts $
  hdr ++ "\n" ++ replicate (length hdr) '-' ++ "\n" ++ cnt ++ "\n"

--- Show additional information.
showDetail :: Options -> String -> a -> IO ()
showDetail opts hdr val = detail opts $
  hdr ++ "\n" ++ replicate (length hdr) '-' ++ "\n" ++ (const "" $!! val)

-- ---------------------------------------------------------------------------
-- 1. Extract all PEVAL annotated expressions and functional patterns.
--    This is a simple structural recursion on the AST.
-- ---------------------------------------------------------------------------

--- Extract all expressions `e` embedded in a call `PEVAL e` in the given
--- program and replaces these calls by `e`.
extractPeval :: Options -> Prog -> (Prog, [Expr])
extractPeval opts (Prog m is ts oldfs ops) = (Prog m is ts newfs ops, concat es)
  where (newfs, es) = unzip (map (extractFunc opts) oldfs)

--- Extract all annotated expressions in a function declaration.
extractFunc :: Options -> FuncDecl -> (FuncDecl, [Expr])
extractFunc opts (Func n a v ty r) = first (Func n a v ty) (extractRule opts r)

--- Extract all annotated expressions in a function rule.
extractRule :: Options -> Rule -> (Rule, [Expr])
extractRule _    e@(External _) = (e, [])
extractRule opts   (Rule  vs e) = first (Rule vs) (extractExpr opts e)

--- Extract all annotated expressions in an expression.
extractExpr :: Options -> Expr -> (Expr, [Expr])
extractExpr _    v@(Var        _) = (v, [])
extractExpr _    l@(Lit        _) = (l, [])
extractExpr opts c@(Comb ct n es) = case getPevalTarget opts c of
  Just e  -> (e, [e])
  Nothing -> (Comb ct n *** concat) (unzip (map (extractExpr opts) es))
extractExpr opts f@(Free    vs e) = case getPevalTarget opts f of
  Just fp -> (f, [fp])
  Nothing -> first (Free vs) (extractExpr opts e)
extractExpr opts o@(Or     e1 e2) = case getPevalTarget opts o of
  Just fp -> (o, [fp])
  Nothing -> let (ne1, pe1) = extractExpr opts e1
                 (ne2, pe2) = extractExpr opts e2
             in  (Or ne1 ne2, pe1 ++ pe2)
extractExpr opts (Case   ct e bs) = let (ne , pe1) = extractExpr opts e
                                        (nbs, pe2) = unzip (map getBranch bs)
                                    in  (Case ct ne nbs, concat (pe1 : pe2))
  where getBranch (Branch p be) = first (Branch p) (extractExpr opts be)
extractExpr opts (Let       bs e) = let (ne , pe1) = extractExpr opts e
                                        (nbs, pe2) = unzip (map getBinding bs)
                                    in  (Let nbs ne, concat (pe1 : pe2))
  where getBinding (v, ve) = first (\e' -> (v, e')) (extractExpr opts ve)
extractExpr opts (Typed     e ty) = first (flip Typed ty) (extractExpr opts e)

--- Check if the expression should be evaluated.
getPevalTarget :: Options -> Expr -> Maybe Expr
getPevalTarget opts e = case e of
  Comb FuncCall (_, "PEVAL") [e'] -> Just e'
  _ | optFunPats opts && hasFP e  -> Just e
    | otherwise                   -> Nothing

--- Is an expression a (combination of) function pattern?
hasFP :: Expr -> Bool
hasFP e = case e of
  Comb FuncCall ("Prelude", "=:<=") _       -> True
  Comb FuncCall ("Prelude", "&"   ) [e1, _] -> hasFP e1
  Comb FuncCall ("Prelude", "cond") [e1, _] -> hasFP e1
  Free _ e1                                 -> hasFP e1
  Or e1 e2                                  -> hasFP e1 || hasFP e2
  _                                         -> False

-- ---------------------------------------------------------------------------
-- 2. Perform partial evaluation
-- ---------------------------------------------------------------------------

--- Partial evaluation of a list of expressions w.r.t. a given program.
--- (e1, e2) => expression `e1` was partially evaluated to expression `e2`
pevalProg :: Options -> Prog -> [Expr] -> [(Expr, Expr)]
pevalProg opts p = reverse . pevalI [] . abstract opts p [] . map topSQ . reverse
  where
  -- global iterative algorithm for PE (fix-point iteration).
  -- @param cache: All expressions and their evaluation
  -- @param s    :The set of already evaluated expressions
  -- @param es   : The list of expressions to be evaluated
  pevalI cache es
    -- termination criteria
    | es == es' = cache'
    | otherwise = pevalI cache' es'
    where
    -- The new cache, extends the old one
    cache' = foldl update cache es
    -- The new set of expressions
    es'    = abstract opts p es (map snd cache' \\ map snd cache)

  -- peval of a list of expressions
  update cache e = case lookup e cache of
    Nothing -> (e, tracePeval e) : cache
    Just _  -> cache

  tracePeval e = traceExp False "Evaluating expression" e
                (traceDebug opts (colorWith opts green "using derivation")
                (traceExp True  "to result expression"  e' $!! e'))
    where
    e'        = pevalExpr opts p e
    pevalExpr = case optSemantics opts of
      RLNT    -> RLNT.pevalExpr
      Natural -> Natural.pevalExpr
      LetRW   -> LetRW.pevalExpr
    traceExp nl str x = Output.traceDetail opts $ colorWith opts green
                      $ pPrint (indent (text str $$ ppExp x))
                        ++ if nl then "\n" else ""

-- ---------------------------------------------------------------------------
-- 3. Building resultants
-- ---------------------------------------------------------------------------

--- Create the initial renaming and resultants from the partially evaluated
--- expressions.
createResultants :: String -> [(Expr, Expr)] -> (Renaming, [Resultant])
createResultants m = unzip . zipWith create [0 ..] . filter madeProgress
  where
  -- We ignore expressions that did not make any progress.
  -- The first part corresponds to expressions that could not be evaluated at
  -- all (for instance: variables, primitive functions such as (v1 + v2), ...).
  -- The second parts corresponds to expressions that evaluate to themselves,
  -- either because its a loop or because of missing information inside
  -- constructor calls (see test/base/double_case for an example).
  madeProgress (e, e') = e /= e' && topSQ e /= e'
  -- Create a new name for the resultants to build
  create n (e, e') = ((e, lhs), (lhs, e'))
    where lhs  = ((m, "_pe" ++ show n), freeVars e)

--- Rename the resultants by (recursively) applying the renaming.
renameResultants :: Options -> Renaming -> [Resultant] -> [Resultant]
renameResultants opts ren = map (renameResultant . second (renameExpr opts ren))

-- ---------------------------------------------------------------------------
-- 5. Post-Unfolding
-- ---------------------------------------------------------------------------

--- Compress the remaining resultants to eliminate intermediate functions.
postUnfolding :: Options -> [Expr] -> Renaming -> [Resultant]
              -> (Renaming, [Resultant])
postUnfolding opts es ren rs = postUnfold opts fs ren rs
  -- fs contains the names of those functions that are called by the
  -- specialisation of the annotated expressions. These functions can not be
  -- removed during postUnfolding.
  where fs = nub $ funcsInExps $ map (renameExpr opts ren . topSQ) es

-- -----------------------------------------------------------------------------
-- 7. Change the names of the resultants
-- -----------------------------------------------------------------------------

--- Changes the names of the given resultants to "pe_0", "pe_1" etc. and
--- adjusts the renaming accordingly.
nameChange :: String -> Renaming -> [Resultant] -> (Renaming, [Resultant])
nameChange mid ren res = (ncRenaming nc ren, ncResultants nc res)
  where nc = zip (map (fst . fst) res) [(mid, "_pe" ++ show i) | i <- [0 ..]]

-- -----------------------------------------------------------------------------
-- 8. Integrate the partially evaluated function declarations.
-- -----------------------------------------------------------------------------

--- Add all function declarations from the partially evaluated program
--- to the original program and replace the PEVAL calls in the original
--- program by the renamed pe-functions calls.
integratePeval :: Options -> Prog -> Renaming -> [FuncDecl] -> Prog
integratePeval opts (Prog m is ts fs ops) ren newfs = Prog m is ts fs' ops
  where fs' = map (replaceFunc (opts, ren)) fs ++ newfs

--- replace all PEVAL annotations by renamed expressions in a function.
replaceFunc :: (Options, Renaming) -> FuncDecl -> FuncDecl
replaceFunc env (Func n a v ty r) = Func n a v ty (replaceRule env r)

--- replace all PEVAL annotations by renamed expressions in a rule.
replaceRule :: (Options, Renaming) -> Rule -> Rule
replaceRule _   e@(External _) = e
replaceRule env (Rule    vs e) = Rule vs (replace env e)

--- replace all PEVAL annotations by renamed expressions in an expression.
replace :: (Options, Renaming) -> Expr -> Expr
replace _   v@(Var        _) = v
replace _   l@(Lit        _) = l
replace env c@(Comb ct n es) = case getPevalTarget (fst env) c of
  Just e' -> uncurry renameExpr env (topSQ e')
  Nothing -> Comb ct n (map (replace env) es)
replace env f@(Free    vs e) = case getPevalTarget (fst env) f of
  Just e' -> uncurry renameExpr env (topSQ e')
  Nothing -> Free vs (replace env e)
replace env o@(Or     e1 e2) = case getPevalTarget (fst env) o of
  Just e' -> uncurry renameExpr env (topSQ e')
  Nothing -> Or (replace env e1) (replace env e2)
replace env (Case   ct e bs) = Case ct (replace env e)
                                       (replace env `onBranchExps` bs)
replace env (Let       bs e) = Let     (map (second (replace env)) bs)
                                       (replace env e)
replace env (Typed     e ty) = Typed   (replace env e) ty

-- -----------------------------------------------------------------------------
-- Helper: Convert resultants to type-inferrec function declarations
-- ---------------------------------------------------------------------------

--- Create function declarations for a list of resultants
createFunctions :: Prog -> [Resultant] -> IO [FuncDecl]
createFunctions p rs = inferTypes p (map resultant2fundecl rs)

--- Infer the types for a list of function w.r.t. a given program.
inferTypes :: Prog -> [FuncDecl] -> IO [FuncDecl]
inferTypes p fs = inferNewFunctions p fs >>= \res -> case res of
  Left  err -> error $ "Error during type inference: " ++ err
  Right fs' -> return (map unAnnFuncDecl fs')

--- Transform a single resultant into a function declaration.
resultant2fundecl :: Resultant -> FuncDecl
resultant2fundecl ((f, vs), rhs)
  = renameFuncDecl $ Func f arity Private ty (Rule vs rhs)
  where
  arity = length vs
  ty    = foldr1 FuncType $ map TVar [0 .. arity]
