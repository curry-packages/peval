
const x _ = x
id x = x

goal1 x = PEVAL (let f = const 0; g = const 1 in (g (), f()))
goal2 x = PEVAL (let g = const 1; f = const 0 in (g (), f()))
