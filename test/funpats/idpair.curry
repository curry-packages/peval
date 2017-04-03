idpair x = (x, x)

f (idpair x) = 0

main = f (idpair True) ? f (True, False)
