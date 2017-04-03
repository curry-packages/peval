PEVAL x = x

[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

main = PEVAL (let xs free in [] ++ xs)
