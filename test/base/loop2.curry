PEVAL x = x

loop = loop

False && _ = False
True  && x = x

main = PEVAL (loop && False)
