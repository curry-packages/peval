double x = x + x

not True = False
not False = True

xorSelf x = xor x x

xor True  x = not x
xor False x = x

-- main x = PEVAL (double (case x of () -> 0 ? 1))
main x = PEVAL (xorSelf (case x of True -> True ? False))
