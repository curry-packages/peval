double x = (x, x)

goal x = PEVAL (let y = case x of () -> (0 ? 1) in double y)

main = goal ()
