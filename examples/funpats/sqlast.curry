[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

goal (_ ++ [x]) = x * x

main = goal [1,2,3]
