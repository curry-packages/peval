PEVAL x = x

[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

main1 xs ys zs = PEVAL (xs ++ (ys ++ zs))

main2 xs ys zs = PEVAL ((xs ++ ys) ++ zs)
