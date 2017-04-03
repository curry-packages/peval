(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

fstFP ([] ++ []) [] = 42

main = fstFP [] [] ? fstFP [1] []
