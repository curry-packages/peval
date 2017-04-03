(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

twoFP ([] ++ []) ([] ++ []) = 42

main = twoFP [] [] ? twoFP [1] [1]
