(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

sndFP [] ([] ++ []) = 42

main = sndFP [] [] ? sndFP [] [1]
