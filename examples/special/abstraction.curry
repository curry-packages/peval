f True  n = f True  (n + 1)
f False n = f False (n + 1)

goal1 = PEVAL (f True  1)
goal2 = PEVAL (f False 1)
