
data Tree a = Leaf a | Branch (Tree a) (Tree a)

leaf   x   = Leaf x
branch a b = Branch a b

mirror l@(leaf   x) = l
mirror (branch a b) = branch (mirror b) (mirror a)

main = mirror (branch (leaf 1) (branch (leaf 2) (leaf 3)))
