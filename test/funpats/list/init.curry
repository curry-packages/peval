PEVAL x = x

[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

init (xs ++ [_]) = xs

main = init [1,2,3]
