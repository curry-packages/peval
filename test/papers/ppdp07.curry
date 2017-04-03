PEVAL x = x

coin = 0
coin = 1

repeat x = x : repeat x

heads (x:y:_) = (x, y)

main = PEVAL (heads (repeat coin))
