import Debug.Profile

(++) :: [t] -> [t] -> [t]
[]     ++ x  = x
(x:xs) ++ ys = x : (xs ++ ys)

goal x y z = PEVAL ((x ++ y) ++ z)

main = goal [1..3] [4..6] [7..10]

benchmark = do
  let l1,l2 free
  doSolve (l1 =:= [1 .. 200000])
  doSolve (l2 =:= [1 .. 100])
  profileTimeNF (goal l1 l2 l2)
