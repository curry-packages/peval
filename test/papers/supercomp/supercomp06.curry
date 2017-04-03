-- This example requires abstraction!

count n = n : count (n + 1)

goal = PEVAL (count 0)

main = take 10 goal
