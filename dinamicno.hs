-- kocke 

steviloStolpov :: Integer -> Integer
steviloStolpov n = steviloModrih n + steviloRdecih n

steviloModrih :: Integer -> Integer
steviloModrih n
    |n < 0 = 0
    |n == 0 = 1
steviloModrih n = steviloRdecih (n-1) + steviloRdecih (n-2)

steviloRdecih :: Integer -> Integer
steviloRdecih n
    |n < 0 = 0
    |n == 0 = 1
steviloRdecih n = steviloModrih (n-2) + steviloModrih (n-3)



-- palindromi 

palindrom :: String -> String
palindrom [x] = [x]
palindrom sez
    |head sez == last sez = [head sez] ++ palindrom (init $ tail sez) ++ [last sez]
    |otherwise = if (length levi) > (length desni) then levi else desni
        where levi = palindrom $ init $ sez
              desni = palindrom $ tail $ sez

-- nahrbtnik
--
-- Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
-- poljubne izdelke, katerih skupna masa ne presega k kilogramov. (Podelili so
-- tri nagrade in sicer s parametrom k = 1, k = 2 in k = 5). Napišite funkcijo
-- nahrbtnik(seznam_artiklov, k), ki poišče največjo ceno, ki jo lahko odnesemo
-- iz trgovine. Naredite dve verziji: pri prvi lahko vzamemo samo en artikel iste
-- vrste, pri drugi pa poljubno število artiklov iste vrste.

izdelki :: [(String, Double, Double)]
izdelki = [("jogurt", 0.39, 0.18),("mleko", 0.89, 1.03),("kava", 2.19, 0.2),("maslo", 1.49, 0.25),("kvas", 0.22, 0.042),("jajca", 2.39, 0.69),
           ("klobasa", 3.76, 0.50),("čebula", 1.29, 2.0),("kruh", 2.99, 1.0),("Nutella", 4.99, 0.75),("sok", 1.15, 2.0)]


nahrbtnik :: [(String, Double, Double)] -> Double -> Double
nahrbtnik [] _ = 0
nahrbtnik _ 0 = 0
nahrbtnik ((_, cena, teza):xs) maxTeza
    |teza > maxTeza = nahrbtnik xs maxTeza
    |otherwise = max cenaZ cenaBrez
        where cenaPreostanka = nahrbtnik xs (maxTeza - teza)
              cenaZ = (cena + cenaPreostanka)
              cenaBrez = nahrbtnik xs maxTeza



-- Miška
-- =====
--
-- Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
-- samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
-- desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
-- različne mase. Miška bi se rada kar se da nažrla, zato jo zanima, katero pot
-- naj ubere. Napišite funkcijo max_sircka(matrika_sirckov), ko dobi matriko z
-- masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
-- optimalni poti.

--def max_sircka(matrika_sirckov):
--    vrstice = len(matrika_sirckov)
--    stolpci = len (matrika_sirckov[0])
--    # v matrika rezultatov shranimo rezultat, če poklicemo max_sircka
--    matrika_rezultatov = [[0 for _ in range(stolpci)] for _ in range (vrstice)]
--    #izpolnimo matriko
--    for vr in range(vrstice -1, -1, -1):
--        for st in range(stolpci -1, -1, -1,):
 --           if vr == vrstice -1 and st == stolpci -1: #kot
--                matrika_rezultatov[vr][st] = matrika_sirckov [vr][st]
--            elif vr == vrstice -1: #dno
 --               matrika_rezultatov[vr][st]=matrika_sirckov[vr][st] + matrika_rezultatov[vr][st + 1]
--            elif st == stolpci -1: #rob
 --               matrika_rezultatov[vr][st] = matrika_sirckov[vr][st] + matrika_rezultatov[vr +1][st]
 --           else: #smo nekje vmes
--                matrika_rezultatov[vr][st] = matrika_sirckov [vr][st] + max (matrika_rezultatov[vr + 1][st], matrika_rezultatov[vr][st +1])
--    return matrika_rezultatov[0][0]

	
-- maxSircka :: [[Int]] -> Int
-- maxSircka [] = 0
-- maxSircka [[]] = 0
-- maxSircka ((x:xs):ys) =
    -- let stVrstic = length ((x:xs):ys)
        -- stStolpcev = length (x:xs)
    -- in 
        -- head $ head [[f i j | j <- [1..stVrstic]] | i <- [0..stStolpcev]]
            -- where f stStolpcev stVrstic = last $ last ((x:xs):ys)
                  -- f s stVrstic = ys !! (s-1) + f (s+1) stVrstic
                  -- f stStolpcev v = last (((x:xs):ys) !! (v-1)) + f stStolpcev (v+1)
                  -- f s v = f s v + f s (v+1) + f (s+1) v

f :: Int -> Int -> [[Int]] -> Int
f vr st matrika@((x:xs):ys)
    |vr == zadnjaVr && st == zadnjiSt = matrika [vr][st]
    |vr == zadnjaVr = matrika[vr][st] + f vr (st + 1) matrika
    |st == zadnjiSt = matrika[vr][st] + f (vr + 1) st matrika
    |otherwise = matrika[vr][st] + max (f (vr + 1) st matrika) (f vr (st + 1) matrika)
    where
        zadnjaVr = length(matrika)
        zadnjiSt = length(matrika)

                  
-- maxSircka :: [[Int]] -> Int
-- maxSircka [] = 0
-- maxSircka [[]] = 0
-- maxSircka matrika@((x:xs):ys) =
    -- let zadnjaVr = length matrika
        -- zadnjiSt = length (x:xs)
    
    -- in f 0 0  
        -- where f vr st 
            -- |vr == zadnjaVr && st == zadnjiSt = matrika [vr][st]
            -- |vr == zadnjaVr = matrika[vr][st] + f vr (st + 1)
            -- |st == zadnjiSt = matrika[vr][st] + f (vr + 1) st
            -- |otherwise = matrika[vr][st] + max (f (vr + 1) st) (f vr (st + 1))

-- Jajca
-- =====
--
-- Živimo v visoki stolpnici, ki ima n nadstropij. Imamo škatlo k jajc, ki so menda zelo trpežna,
-- saj naj bi prenesla padce z višjih nadstropij stoplnice. Radi bi ugotovili, katero je najvišje
-- nadstopje, pri katerem jajca še preživijo padec. Ker nimamo veliko časa, bi radi poiskali
-- strategijo, pri kateri bomo minimizirali število metov.
--
-- Razmislite:
--  * Kako moramo ravnati v primeru, ko imamo samo eno jajce?
--  * Kako lahko ravnamo v primeru, ko imamo na voljo zelo veliko jajc (več kot je število
--    nadstropij)?
--
-- Napišite funkcij, ki bo izračunala maksimalno število metov (v najslabšem primeru), da ugotovimo
-- številko kritičnega nadstropja, če imamo na voljo točko k jajc.

maxMetov :: Int -> Int -> Int
maxMetov 0 _ = 0
maxMetov n k
    |n == 1 && k >= 1 = 1
    |k == 1 = n
maxMetov n k = minimum [max (razbije z) (neRazbije z) | z <- [1..n] ] 
    where razbije z = 1 + maxMetov (z) (k-1)
          neRazbije z = 1 + maxMetov (n-z-1) k