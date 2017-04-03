[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

goal (xs ++ _) = xs

main = goal [1,2,3]
