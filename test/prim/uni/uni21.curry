PEVAL x = x

goal x = PEVAL (let y free in y =:= x &> y)

main = goal True ? goal failed
