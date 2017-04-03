-- This example has been taken from
--   William R. Cook and Ralf Lämmel:
--   Tutorial on Online Partial Evaluation,
--   to appear in proceedings of DSL 2011

type State  = Int
type Label  = Char
type Trans  = [(State, [(Label, State)])]
type Accept = [State]

transitions = [(1, [('a', 2)]), (2, [('b', 1)])]
accept      = [2]

elem _ []     = False
elem x (y:ys) = if x == y then True else elem x ys

lookup _ []           = Nothing
lookup x ((k, v):kvs) = if x == k then Just v else lookup x kvs

run :: State -> Accept -> Trans -> [Label] -> Bool
run cur accpt _     []     = cur `elem` accpt
run cur accpt trans (l:ls) = case lookup cur trans of
  Nothing -> False
  Just t  -> case lookup l t of
    Nothing   -> False
    Just next -> run next accpt trans ls

goal ls = PEVAL (run 1 accept transitions ls)

main = map goal ["a", "aba", "ababa", "b", ""]
