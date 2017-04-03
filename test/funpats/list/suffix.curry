[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

goal (_ ++ xs) = xs

main = goal [1,2,3]
