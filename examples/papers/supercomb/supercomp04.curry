goal x = PEVAL (fcase x of 3 -> x + x ; 4 -> x * x)

main = goal (2 ? 3 ? 4)
