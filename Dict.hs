{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- We implement dictionaries using naïve binary search trees.
module Dict
  ( Dict
  , empty
  , add
  , search
  , remove
  , fromList
  , internalTests
  )
  where

import Test.QuickCheck
import Control.Monad

data Tree a = Leaf | Node (Tree a) a (Tree a)  deriving (Eq, Show)

type Dict k v = Tree (k, v)

-- An empty dictionary.
empty :: Dict k v
empty = Leaf

-- UČINKOVITO,  A napačno JAVI, DA PATTERN MATCHING NOT EXHAUSTIVE 
-- [add k v d] associates [v] to [k] in a dictionary [d].
-- add' :: Ord k => k -> v -> Dict k v -> Dict k v
-- add' k v Leaf = Node Leaf (k, v) Leaf
-- add' k v (Node l a @(k', _) r) 
    -- | k < k' = Node (add k v l) a r
    -- | k == k' = Node l (k, v) r 
    -- | k' < k = Node l a (add k v r)

-- ZKEJ SPODNJA KODA N DEJLA???
-- add k v (Node Left a@(k', v') Right)
    -- |k' == k = Node Left (k, v') Right
    -- |k' > k = Node (add k v Left) (k', v') Right
    -- |k' < k = Node Left (k', v') (add k v Right)

add :: Ord k => k -> v -> Dict k v -> Dict k v    
add k v Leaf = Node Leaf (k, v) Leaf
add k v (Node l a@(k', _) r) = 
    case compare k k' of
        LT -> Node (add k v l) a r 
        EQ -> Node l (k, v) r
        GT -> Node l  a (add k v r)

-- [search k d] finds the value associated to a key [k], if it is present in a
-- dictionary.
search :: Ord k => k -> Dict k v -> Maybe v
search k Leaf = Nothing
search k (Node l a@(k', v') r) = 
    case compare k k' of
        LT -> search k l
        EQ -> Just v'
        GT -> search k r 

-- [insert_sub_r sub t] inserts [sub] as a the rightmost subtree in [t].
-- insertSubR :: Tree a -> Tree a -> Tree a
-- insertSubR Leaf Leaf = Leaf
-- insertSubR (Node l a r) Leaf = Node l a r
-- insertSubR (Node l a r) (Node l' a r') = undefined

-- [remove k d] returns [d] where [k] does not have an associated value.
remove :: Ord k => k -> Dict k v -> Dict k v
remove = undefined

-- Produce a dictionary from a list. The keys in the list are assumed to be
-- without repetition.
fromList :: Ord k => [(k,v)] -> Dict k v
fromList = undefined

depth :: Tree a -> Integer
depth = undefined

-- We provide a generator for random trees for QuickCheck.
arbitraryTree :: (Arbitrary a) => Gen (Tree a)
arbitraryTree = sized genTree
    where genTree n = case compare n 0 of
            LT -> return Leaf
            EQ -> return Leaf
            GT -> oneof [return Leaf,
                         liftM3 Node subtree arbitrary subtree]
              where subtree = genTree (n `div` 2)

-- ...but we don't use it, because it does not generate search trees.
-- instance Arbitrary a => Arbitrary (Tree a) where
--   arbitrary = arbitraryTree

-- Instead, here's a generator for search trees.

-- Note that the reliability and significance of all the tests that use a
-- generator G rely on the correctness of G. Here, we rely on [fromList] and
-- thus on [add]. We could instead write a potentially complex generator.
genSearchTree :: (Ord k, Arbitrary k, Arbitrary v) => Gen (Dict k v)
genSearchTree = liftM fromList arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Dict k v) where
    arbitrary = genSearchTree

isSorted :: (Ord k) => Dict k v -> Bool
isSorted d = isSortedBetween Nothing d Nothing where
  isSortedBetween _ Leaf _ = True
  isSortedBetween minK (Node l (k, _) r) maxK =
    minK <=? Just k && Just k <=? maxK &&
    isSortedBetween minK l (Just k) &&
    isSortedBetween (Just k) r maxK
  Nothing <=? _ = True
  _ <=? Nothing = True
  Just x <=? Just y = x <= y

-- This should always hold if the generator uses [add] internally.
prop_isSortedAdd :: Ord k => Dict k v -> k -> v -> Property
prop_isSortedAdd d k v =
    collect (depth d) $ collect (isSorted d) $ (isSorted d) ==> (isSorted (add k v d))

internalTests :: IO ()
internalTests = do
  quickCheck (isSorted :: Dict Integer String -> Bool)
  quickCheck (isSorted :: Dict String Integer -> Bool)
  quickCheck (prop_isSortedAdd :: Dict Integer String -> Integer -> String -> Property)
  quickCheck (prop_isSortedAdd :: Dict String Integer -> String -> Integer -> Property)
