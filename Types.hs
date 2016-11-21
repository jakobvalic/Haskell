{-
 - Exercise set 3: Datatypes
 -}

-- Natural numbers
-- ===============

data Natural = Zero | Succ Natural deriving (Show)

-- 'add m n' returns the sum of natural numbers 'm' and 'n'

add :: Natural -> Natural -> Natural
add Zero n = n
add (Succ x) n =  add x (Succ n) -- m je Succ x


-- 'multiply m n' returns the product of natural numbers 'm' and 'n'

multiply :: Natural -> Natural -> Natural
multiply Zero _ = Zero
multiply (Succ x) n = add n (multiply x n)

-- 'toNatural n' converts an integer 'n' into a natural number
-- 
-- Example:
-- ghci> toNatural 0
-- Zero
-- ghci> toNatural 2
-- Succ (Succ Zero)

toNatural :: Integer -> Natural
toNatural 0 = Zero
toNatural n = Succ (toNatural (n - 1))

-- 'fromNatural n' converts a natural number 'n' to an integer
-- 
-- Example:
-- ghci> fromNatural Zero
-- 0
-- ghci> fromNatural (Succ (Succ Zero))
-- 2

-- fromNatural :: Natural -> Integer
-- fromNatural Zero = 0
-- fromNatural (Succ x) = 1 + fromNatural x

-- idNat :: Natural -> Natural
--idNat n = toNatural (fromNatural n)

-- idInt :: Int -> Int
-- idInt x = fromNatural(toNatural x)

-- Trees
-- =====

-- Here we define the recursive datatype Tree. We will add more functions that
-- work on trees, like the sumTree example, which calculates the sum of the
-- elements of a tree.

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node x left right) = x + sumTree left + sumTree right
-- vsota (Node vrednost levo desno) = vrednost + vsota levo + vsota desno

-- 'depth tr' returns the depth of a tree. Leaves have depth 0.
--
-- Example:
-- ghci> let d = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> globina d
-- 3

depth :: Tree a -> Int --moraš dati Tree a
depth Leaf = 0
depth (Node x l r) = 1 + max (depth l) (depth r)

-- 'numberOfElements tr', for 'tr' of type 'Tree alpha' computes the number of
-- alpha's ("elements") in tr.
--
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> numberOfElements tr
-- 4

numberOfElements = undefined

-- 'treeFlip tr' swaps the left and right subtrees of each node in 'tr'
--
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> flip tr
-- Node 3 (Node 8 Leaf Leaf) (Node 7 (Node 2 Leaf Leaf) Leaf)

treeFlip :: Tree a -> Tree a
treeFlip Leaf = Leaf
treeFlip (Node x l r)  = Node x (treeFlip r) (treeFlip l)

-- 'leftMost tr' returns the left-most element in 'tr'.
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> leftMost tr
-- 7

leftMost :: Tree a -> Maybe a -- Maybe za primer praznih dreves, za primer, ko NI DEFINIRANO
leftMost Leaf = Nothing
leftMost (Node x Leaf r) = Just x -- Zaradi tipa Maybe
leftMost (Node x l r) =  leftMost l



-- Complex Numbers
-- ==================

-- We have defined a datatype Complex, which represents complex numbers. We
-- will add some functions to work with complex numbers.

data Complex = Complex Double Double deriving (Show)

-- 're x' returns the real part of the complex number x.

re :: Complex -> Double
re (Complex realni imaginarni) = realni

-- 'im x' returns the imaginary part of the complex number x.

im :: Complex -> Double
im (Complex ralni imaginarni) = imaginarni

-- 'conjugate x' returns the complex conjugate of x.

conjugate :: Complex -> Complex
conjugate (Complex realni imaginarni) = Complex realni (-imaginarni)


-- Polynomials
-- ===========

-- We define a datatype Polynomial, which represents polynomials in one
-- variable over the field of rationals, with coefficients in increasing order.
-- We will add more functions to work with polynomials.

data Polynomial = Polynomial [Rational] deriving (Show)

-- Example: the polynomial x^1 + 0
p_x :: Polynomial
p_x = Polynomial [0, 1]


-- 'eval p x' evaluates the polynomial 'p' at point 'x'.
--
-- Example:
-- ghci> let p = Polynomial [2,0,-1]
-- ghci> eval p 2
-- -2

eval = undefined

-- 'derivative p' computes the first derivative of p.

derivative = undefined

-- 'integral p' computes the indefinite integral of p.

integral = undefined
