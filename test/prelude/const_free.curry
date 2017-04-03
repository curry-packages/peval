PEVAL x = x

const x _ = x

main = PEVAL (let x free in const 42 x)
