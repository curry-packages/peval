(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

vars ([x] ++ [y]) = x + y

main = vars [23, 19]
