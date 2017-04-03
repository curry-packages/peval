PEVAL x = x

id x = x

not True  = False
not False = True

app f x = f x

goal x = PEVAL (app (case x of True -> id; False -> not) True)

main = (goal True, goal False)
