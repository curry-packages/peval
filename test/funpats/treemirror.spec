module treemirror
  ( treemirror.Tree (..), treemirror.leaf, treemirror.branch, treemirror.mirror
  , treemirror.main )
  where

import Prelude

data treemirror.Tree a
  = treemirror.Leaf a
  | treemirror.Branch (treemirror.Tree a) (treemirror.Tree a)

treemirror.leaf :: a -> treemirror.Tree a
treemirror.leaf v1 = treemirror.Leaf v1

treemirror.branch :: treemirror.Tree a -> treemirror.Tree a -> treemirror.Tree a
treemirror.branch v1 v2 = treemirror.Branch v1 v2

treemirror.mirror :: treemirror.Tree a -> treemirror.Tree a
treemirror.mirror v1 = treemirror._pe0 v1

treemirror.main :: treemirror.Tree Prelude.Int
treemirror.main = treemirror.mirror
  (treemirror.branch
  (treemirror.leaf 1)
  (treemirror.branch (treemirror.leaf 2) (treemirror.leaf 3)))

treemirror._pe0 :: treemirror.Tree a -> treemirror.Tree a
treemirror._pe0 v1 = fcase v1 of
    treemirror.Leaf v2 -> treemirror.Leaf v2
    treemirror.Branch v3 v4 -> treemirror.Branch
      (treemirror._pe0 v4)
      (treemirror._pe0 v3)
