PEVAL x = x

goal x = PEVAL ((case x of True -> 1; False -> 0) =:= 1)

main = goal True ? goal False
