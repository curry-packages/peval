PEVAL x = x

[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

last (_ ++ [x]) = x

main = last [1,2,3]
