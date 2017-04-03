PEVAL x = x

False && _ = False
True  && x = x

main = PEVAL (unknown && unknown)
