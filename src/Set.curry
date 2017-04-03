--- ----------------------------------------------------------------------------
--- Library for a set datatype with an implementation using red-black trees.
--- This module is intended to be used qualified, i.e.,
---
---    import qualified Set
---    import           Set (Set)
---
--- All the operations are based on the primitive ordering on elements
--- obtained by `(==)` and `(<)`.
---
--- @author Björn Peemöller
--- @version September 2015
--- ----------------------------------------------------------------------------
module Set where

import qualified RedBlackTree as RBT ( RedBlackTree, delete, empty, isEmpty
                                     , lookup, tree2list, update)
import           Maybe               (isJust)

type Set a = RBT.RedBlackTree a

--- Return an empty set.
empty :: Set a
empty = RBT.empty (==) (==) (<)

--- Test for an empty set.
null :: Set _ -> Bool
null = RBT.isEmpty

--- Returns true if an element is contained in a set.
--- @param e - an element to be checked for containment
--- @param s - a set
--- @return True iff e is contained in s
elem :: a -> Set a -> Bool
elem e = isJust . RBT.lookup e

--- Inserts an element into a set if it is not already there.
insert :: a -> Set a -> Set a
insert = RBT.update

--- Delete an element from a set.
delete :: a -> Set a -> Set a
delete = RBT.delete

--- Transforms a set into an ordered list of its elements.
toList :: Set a -> [a]
toList = RBT.tree2list

--- Transforms a list of elements into a set.
fromList :: [a] -> Set a
fromList = foldr insert empty

--- Computes the union of two sets.
union :: Set a -> Set a -> Set a
union s1 = foldr insert s1 . toList

--- Computes the intersection of two sets.
intersect :: Set a -> Set a -> Set a
intersect s1 s2 = fromList $ filter (`elem` s2) $ toList s1

--- Test for disjointness of sets.
disjoint :: Set a -> Set a -> Bool
disjoint s1 s2 = null (intersect s1 s2)
