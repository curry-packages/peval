PEVAL x = x

main = PEVAL (fcase (let x free in x) of True -> 42)
