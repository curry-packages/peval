PEVAL x = x

goal x = PEVAL (case x of True -> x ; False -> x)

main = (goal True, goal False)
