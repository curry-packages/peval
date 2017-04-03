PEVAL x = x

goal x = PEVAL (1 =:= (case x of True -> 1; False -> 0))

main = goal (True ? False)
