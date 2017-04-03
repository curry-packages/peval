enumFT a b = if (a > b) then [] else (a : enumFT (a + 1) b)
--enumFT a b = ite (a>b) [] (a : enumFT (a+1) b)
--ite True  a b = a
--ite False a b = b

goal n = PEVAL (enumFT 1 n)

main = goal 10

-- main "enum" [(Comb FuncCall "enum_g" [Var 0])]
