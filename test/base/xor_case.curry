xor False False = True
xor False True  = False
xor True  False = False
xor True  True  = True

goal x = PEVAL (let y = case x of () -> (False ? True) in xor y y)

main = goal ()
