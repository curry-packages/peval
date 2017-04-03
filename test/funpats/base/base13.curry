(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

nonLinear (xs ++ xs) = 42

main = nonLinear [] ? nonLinear [1] ? nonLinear [1,1]
