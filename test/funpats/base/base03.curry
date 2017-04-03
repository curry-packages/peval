(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

unaryGuard ([] ++ []) | 1 =:= 1 = 42

main = unaryGuard [] ? unaryGuard [1]
