goal x = PEVAL ((case x of True -> 1 ; False -> 2) + 3)

main = goal (True ? False)
