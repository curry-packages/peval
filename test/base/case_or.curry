PEVAL x = x

x ? _ = x
_ ? y = y

main = PEVAL (case (True ? False) of True -> 42)
