PEVAL x = x

head (x:_) = x

not True  = False
not False = True

iterate f x = x : iterate f (f x)

main = PEVAL (head (iterate not True))
