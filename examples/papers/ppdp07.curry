-- This example has been taken from:
--   L\'{o}pez-Fraguas, Francisco Javier and Rodr\'{\i}guez-Hortal\'{a}, Juan and S\'{a}nchez-Hern\'{a}ndez, Jaime
--   A Simple Rewrite Notion for Call-time Choice Semantics
--   PPDP '07

PEVAL x = x

coin = 0
coin = 1

repeat x = x : repeat x

heads (x:y:_) = (x, y)

main = PEVAL (heads (repeat coin))
