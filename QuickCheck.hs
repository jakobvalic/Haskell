import Control.Monad
import Test.QuickCheck


data Drevo a = Prazno
             | Sestavljeno (Drevo a) a (Drevo a)
             deriving (Eq, Show)

              
--                
-- Tole poskrbi, da si QuickCheck zna izmisliti naključno drevo.               
-- 
instance (Arbitrary a) => Arbitrary (Drevo a) where
  arbitrary = sized tree'
    where
      tree' 0 = return Prazno
      tree' n | n > 0 = 
        oneof [return Prazno,
              liftM3 Sestavljeno subtree arbitrary subtree]
        where subtree = tree' (n `div` 2)

       
       
-- Sestavite funkcijo prezrcali, ki med seboj zamenja levo in desno
-- poddrevo (v vseh vozliščih).

prezrcali :: Drevo a -> Drevo a
prezrcali Prazno = Prazno
prezrcali (Sestavljeno l x d) = Sestavljeno (prezrcali d) x (prezrcali l) -- oklepaje moramo pisati natanko tako

-- Sestavite funkcijo globina, ki vrne globino drevesa.

globina :: Drevo a -> Int
globina Prazno = 0
globina (Sestavljeno l _ d) = 1 + max (globina l) (globina d)

-- Sestavite funkcijo vsota, ki vrne vsoto vseh elementov v drevesu.

vsota :: Num a => Drevo a -> a
vsota Prazno = 0
vsota (Sestavljeno l x d) = vsota(l) + x + vsota(d)

-- Če drevo dvakrat prezrcalimo, dobimo spet isto nazaj. Napišite ustrezno
-- lastnost prop_prezrcaliPrezrcali.

prop_prezrcaliPrezrcali :: Eq a => Drevo a -> Bool
prop_prezrcaliPrezrcali d = d == (prezrcali $ prezrcali d)

-- Če drevo prezrcalimo, se mu globina pri tem ne spremeni. Napišite ustrezno
-- lastnost prop_globinaPrezrcali.

prop_globinaPrezrcali :: Drevo a -> Bool
prop_globinaPrezrcali d = globina d == (globina $ prezrcali d)

-- Če drevo prezrcalimo, se mu vsota pri tem ne spremeni. Napišite ustrezno
-- lastnost prop_vsotaPrezrcali.

prop_vsotaPrezrcali :: (Eq a, Num a) => Drevo a -> Bool
prop_vsotaPrezrcali d = vsota d == (vsota $ prezrcali d)

testi1 = do
    quickCheck (prop_prezrcaliPrezrcali :: Drevo Int -> Bool)
    quickCheck (prop_prezrcaliPrezrcali :: Drevo Char -> Bool)
    quickCheck (prop_globinaPrezrcali :: Drevo Int -> Bool)
    quickCheck (prop_globinaPrezrcali :: Drevo Char -> Bool)
    quickCheck (prop_vsotaPrezrcali :: Drevo Int -> Bool)

-- Slovarje predstavimo z asociativnimi seznami.

type Slovar k v = [(k, v)]

-- Definirajte prazen slovar.

prazen :: Slovar k v
prazen = []

-- Definirajte metodo poisci, ki v slovarju poišče vrednost danega ključa.

poisci :: (Eq k) => Slovar k v -> k -> Maybe v
poisci [] _ = Nothing
poisci ((kljuc,vrednost):ostanek) k
    | k == kljuc = Just vrednost
    | otherwise = poisci ostanek k

-- Definirajte metodo dodaj, ki v slovar doda podan ključ in vrednost.

dodaj :: Eq k => Slovar k v -> k -> v -> Slovar k v
dodaj [] k v = [(k,v)]
dodaj ((kljuc, vrednost) : ostanek) k v
    | kljuc == k = (k, v) : ostanek
    | otherwise = (kljuc, vrednost) : dodaj ostanek k v
    
--dodaj (Slovar k v) k2 v2 = Slovar (k ++ k2) (v ++ v2)

-- Če smo v slovar ravnokar vstavili nek ključ in vrednost, moramo dobiti
-- pri iskanju tega ključa vrednost, ki smo jo ravnokar vstavili.

prop_poisciDodaj :: (Eq k, Eq v) => Slovar k v -> k -> v -> Bool
prop_poisciDodaj s k v = (poisci (dodaj s k v) k ) == Just v

-- Če v slovar dodamo k in v, nato pa iščemo nek k', ki je različen od k,
-- bi morali dobiti isto vrednost kot pri iskanju v starem slovarju.

prop_poisciDodaj2 :: (Eq k, Eq v) => Slovar k v -> k -> v -> k -> Property
prop_poisciDodaj2 s k v k' = (k /= k') ==> (poisci (dodaj s k v) k') == (poisci s k')

testi2 = do
    quickCheck (prop_poisciDodaj :: Slovar Int Int -> Int -> Int -> Bool)
    quickCheck (prop_poisciDodaj :: Slovar Int String -> Int -> String -> Bool)
    quickCheck (prop_poisciDodaj2 ::  Slovar Int Int -> Int -> Int -> Int -> Property)
    quickCheck (prop_poisciDodaj2 ::  Slovar Int String -> Int -> String -> Int -> Property)

main = do
  testi1
  testi2