(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

unary ([] ++ [])= 42

main = unary [] ? unary [1]
