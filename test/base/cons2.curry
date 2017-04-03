m1 = Just 0
m2 = Nothing

[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

main = PEVAL ([m1] ++ [m2])
