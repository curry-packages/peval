PEVAL x = x

[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

goal xs ys zs = PEVAL ((xs ++ ys) ++ zs)

main = goal [1..3] [4..6] [7..10]
