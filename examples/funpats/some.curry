PEVAL x = x

[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

some (_ ++ (x:_)) = x

main = some [1,2,3]
