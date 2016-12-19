-- AVL trees

{- resources:
   - https://visualgo.net/bst  nice visualisation of all operations, recommended!

   the English wiki article on AVL trees is very poorly written; the article on
   rotations is worthwhile
   - https://de.wikipedia.org/wiki/AVL-Baum  brush up on your German! or use
     google translate
   - https://en.wikipedia.org/wiki/Tree_rotation
-}


data AVL a = Leaf | Node Int (AVL a) a (AVL a) deriving Show

-- here's a drawing function, which, combined with [putStrLn], lets you visualise a tree
data S = L | R
draw :: Show a => AVL a -> String
draw t = "\n" ++ draw' Nothing t 0 ++ "\n"
  where
    draw' _ Leaf _ = []
    draw' dir (Node _ l v r) d =
      draw' (Just R) r (d+1) ++ node dir ++ draw' (Just L) l (d+1)
      where
        node dir' = padding d ++
          case dir' of
            Nothing -> ""
            Just L -> "\\- "
            Just R -> "/- "
          ++ show v ++ "\n"
        padding n = replicate (n*4) ' '


empty :: AVL a
empty = Leaf

height :: AVL a -> Int
height Leaf = 0
height (Node h _ _ _) = h

-- [avl left a right] is a smart constructor that fills in the height of the
-- new tree. The height of [left] and [right] may differ by at most 1
avl :: AVL a -> a -> AVL a -> AVL a
avl tree1 a tree2  = Node h tree1 a tree2
    where h = 1 + max (height tree1) (height tree2)

-- check if an element appears in a tree
element :: Ord a => a -> AVL a -> Bool
element x Leaf = False
element x (Node _ l a r)
    |x == a = True
    |x > a = element x r
    |otherwise = element x l



-- [skew t] computes how "disbalanced" [t] is, ie. how its subtrees compare in
-- height. A search tree is an AVL tree is this factor is never greater than 1.
skew :: AVL a -> Int
skew Leaf = 0
skew (Node _ l _ r) = height l - height r

-- Perform a right rotation
rotR :: AVL a -> AVL a
rotR Leaf = Leaf
rotR (Node h (Node _ l' a' r') a r) = newTree
    --where newTree = avl l' a' (Node (h - 1) r' a r)
    where newTree = Node h l' a' (avl r' a r)

-- Perform a left rotation
rotL :: AVL a -> AVL a
rotL Leaf = Leaf
rotL (Node h l a (Node _ l' a' r')) = Node h (avl l a l') a' r'

-- [rebalance t] performs the rotations necessary to re-balance an almost-AVL
-- tree which is skewed by at most 2.
rebalance :: AVL a -> AVL a
rebalance Leaf = Leaf
rebalance m@(Node _ l x d)
    | skew m == 2 && skew l == 1 = rotR m
    | skew m == 2 = rotR $ avl (rotL l) x d
    | skew m == -2 && skew d == -1 = rotL m
    | skew m == -2 = rotL $ avl l x (rotR d)
    | otherwise = m


-- [add t a] inserts an element into an AVL tree, ensuring that the invariants
-- are respected.
add :: (Ord a) => AVL a -> a -> AVL a
add Leaf x = Node 1 Leaf x Leaf
add (Node h l a r) x
    |x == a = Node h l a r
    |x > a = rebalance $ Node h l a (add r x)
    |otherwise = rebalance $ Node h (add l x) a r

-- With [add] implemented, we can easily construct an AVL tree from a list
fromList :: (Ord a) => [a] -> AVL a
fromList = undefined

-- [minAVL t] finds the minimum element of [t]
minAVL :: (Ord a) => AVL a -> Maybe a
minAVL = undefined

-- [delete t a] returns an AVL tree [t] with [a] removed
delete :: Ord a => AVL a -> a -> AVL a
delete = undefined
