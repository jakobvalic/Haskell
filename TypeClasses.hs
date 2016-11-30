{-# LANGUAGE MultiParamTypeClasses #-}

-- Complex Numbers
-- ===============
--
-- Show that the types Natural, Complex and Polynomial of the previous lesson
-- belong to the Num class:

data Natural = Zero | Succ Natural deriving (Show)

data Complex = Complex Double Double deriving (Show)

data Polynomial = Polynomial [Rational] deriving (Show)
    
instance Num Natural where
    Zero + n = n
    (Succ m) + n = Succ (m + n)
    Zero * n = Zero
    (Succ m) * n = m * n + n
    abs Zero = Zero
    abs (Succ m) = Succ m
    signum Zero = Zero
    signum (Succ m) = Succ Zero --ti ga pretvori v Succ Zero
    negate Zero = Zero
    negate n = undefined
    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n - 1))

instance Num Complex where
    (Complex x1 y1) + (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)
    (Complex x1 y1) * (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)
    abs (Complex x y) = Complex (x * x + y * y) 0 --vrnemo v obliki kompleksnega števila
    --signum (Complex x1 y1) = Complex (cos x1 / y1) (sin x1 / y1) --vrnemo točko na enotski krožnici
    fromInteger n = Complex (fromIntegral(n)) 0
    negate (Complex x1 y1) = Complex (-x1) (-y1)

-- vsotaSez :: [a] -> [a] -> [a]    
-- vsotaSez [] y = y
-- vsotaSez x [] = x
-- vsotaSez (x:xs) (y:ys) = x + y + 0 : (xs + ys)  
  
instance Num Polynomial where
    -- Polynomial x + Polynomial [] = Polynomial x
    -- --Polynomial (x:xs) + Polynomial (y:ys) = Polynomial vsota where
    -- [x + y] + 0:(Polynomial xs + Polynomial ys)
    Polynomial [x] * Polynomial ys = Polynomial [x * y | y <- ys]
    Polynomial (x:xs) * Polynomial ys = Polynomial [x] * Polynomial ys + Polynomial (0:zs)
        where 
            Polynomial zs = Polynomial xs * Polynomial ys     
    -- --Polynomial x + Polynomial y = Polynomial [i + j | (i, j) <- zip (x y)]
    signum = error "Polynomial: operation signum does not make sense"


-- Algebraic structures
-- ====================

-- The [Semigroup] class can be defined like this:

class  Semigroup a  where
    (***) :: a -> a -> a

-- Define the following classes (extensions):
-- + SemigroupWithUnit (with a special element "unit")

class (Semigroup a) => SemigroupWithUnit a where
	unit :: a

-- + Group (with an "inv" function)

class SemigroupWithUnit a => Group a where
	inverz :: a -> a
    
-- + Ring

class (Group a) => Ring a where
    (###) :: a -> a -> a --množenje





-- Show that the integers belong to the Ring class    

instance Semigroup Integer where
    n *** m = n + m
    
instance SemigroupWithUnit Integer where
    unit = 0
   
instance Group Integer where
    inverz n = -n
    
instance Ring Integer where
    n ### m = n * m
    
        
-- Show that Bool belongs to Group
-- Bool je grupa za operacijo xOR
instance Semigroup Bool where
	--(***) = (||) --(&&)
    p *** q = p || q

instance SemigroupWithUnit Bool	where
	unit = False --(True)
	
instance Group Bool where
	inverz True = False
	inverz False = True    
    
   



-- Show that the type [Z_2] as defined below belongs to Group

data Z_2 =  Zero_2 | One_2 deriving (Show)

instance Semigroup Z_2 where
    Zero_2 *** Zero_2 = Zero_2
    One_2 *** Zero_2  = One_2
    Zero_2 *** One_2 = One_2
    One_2 *** One_2 = One_2
    
instance SemigroupWithUnit Z_2 where
    unit = Zero_2
    
instance Group z_2 where
    inverz Zero_2 = One_2
    inverz One_2 = Zero_2
    

-- Show that the cartesian product type of two types in the Group class belongs
-- to the Group class


-- Let types [a] and [b] belong to the Group class. To say that [a] and [b] are
-- isomorphic, we can define the Isomorphism class:

class  Isomorphism a b  where
    towards :: a -> b
    backwards :: b -> a

-- Show that [Bool] and [Z_2] are isomorphic as groups



-- Distributions
-- =============

-- We define the data type of distributions:

data Distribution a = Distribution [(a, Rational)] deriving Show

-- Two simple examples:

data Coin = Heads | Tails
coin :: Distribution Coin
coin = Distribution [(Heads, 1/2), (Tails, 1/2)]
die :: Distribution Int
die = Distribution [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]

-- [isDistribution d] checks that d really is a distribution, ie that the
-- probabilities sum up to 1.

isDistribution = undefined

-- [cleanDistribution d] merges together repeated events

cleanDistribution = undefined

-- [mostLikely d] returns the most likely event in [d], and the last one listed
-- if it is not unique.

mostLikely = undefined

-- [uniform d] returns a uniform distribution of the events in d

uniform = undefined

-- [expectation d] returns the expected value of d

expectation = undefined

-- [weightedSum p d1 d2] computes a distribution obtained by merging the
-- (compatible) distributions [d1] and [d2], scaling events in [d1] by
-- the weight [p] and events in [d2] by (1-p).

weightedSum = undefined


-- Show that the type constructor [Distribution] belongs to the [Functor] type
-- class.

instance  Functor Distribution  where
    fmap = undefined



-- Moving in space
-- ===============

-- Define a [Point] type, which should have the parameters (name, X-coordinate,
-- Y-coordinate, Z-coordinate). Implement a [show] function for [Point]

-- The point will move through space. Before we can move, we must first be
-- somewhere. Define a type class [Positioned] with a [currentPosition] value
-- and show that [Point] is a member of [Positioned]

-- Define the [Movable] type class that implements [setNewLocation] and make
-- [Point] a member of it.


-- Za spremenljivke, katerih tip pripada razredu Premakljiv, definirajte
-- funkcijo premakniZa, ki spremenljivko premakne za določen vektor. Ta funkcija
-- bo tako delovala tudi za poljubno točko!

-- ??????????????????????


-- In some species of spiders, the female is known to eat the male after
-- mating. The females which eat the males lay more eggs, which produce
-- stronger and bigger embryos. One tentative explanation is that such females
-- are more aggressive and therefore better hunters. Another theory says that
-- males simply are an excellent source of essential nutrients.
--
-- Imagine a female spider at location (0,0,0). The female wants to eat the
-- male which is at coordinates (3,3,3). Assume that the spiders' movements are
-- confined to a 10x10x10 cube (ie each coordinate is always between 0 and 9).
--
-- We will make both spiders move around. First moves the female, which tries
-- to get as close as possible to the male's position. Then moves the male,
-- which wants to maximize the distance from the female.

-- Write a [move f m] function that maps a position of a female [f] and of a
-- male [m] to a pair of moves.

move = undefined

-- Finally, write [simulate], which simulates the behaviour of a female and a male.

simulate = undefined
