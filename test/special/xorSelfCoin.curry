not True  = False
not False = True

xor True  y = not y
xor False y = y

xorSelf x = xor x x

coin = False
coin = True

main = PEVAL (xorSelf coin)
