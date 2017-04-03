PEVAL x = x

goal = PEVAL (let one = 1 : two; two = 2 : one in one)

main = take 10 goal
