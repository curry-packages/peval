f x = PEVAL (x + fcase x of 0 -> 1; 1 -> 2)

main = (f 0, f 1)
