-- constraint solving (simple generate and test) in Curry:
-- graph coloring

-- negation of =:=
diff :: a -> a -> Success
diff x y = (x == y) =:= False

{-
 This is our actual map:

 ------------------
 |       |        |
 |       |   L2   |
 |       |        |
 |  L1   |--------|
 |       |        |
 |       |   L3   |
 |       |        |
 ------------------
-}

data Color = Red | Green | Yellow

isColor :: Color -> Success
isColor Red    = success
isColor Green  = success
isColor Yellow = success

-- possible colorings:
coloring :: Color -> Color -> Color -> Success
coloring l1 l2 l3 = isColor l1 & isColor l2 & isColor l3

-- correct coloring:
correct :: Color -> Color -> Color -> Success
correct l1 l2 l3 = diff l1 l2 & diff l1 l3 & diff l2 l3

-- generate+test solution:
goal :: Color -> Color -> Color -> Success
goal l1 l2 l3 = PEVAL (coloring l1 l2 l3 & correct l1 l2 l3)

main :: (Color, Color, Color)
main = goal l1 l2 l3 &> (l1, l2, l3) where l1, l2, l3 free
