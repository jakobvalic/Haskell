# Miška
# =====
#
# Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
# samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
# desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
# različne mase. Miška bi se rada kar se da nažrla, zato jo zanima, katero pot
# naj ubere. Napišite funkcijo max_sircka(matrika_sirckov), ko dobi matriko z
# masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
# optimalni poti.

##@lru_cache()
##def max_sircka(matrika_sirckov):
##    #kolicina sira
##    kolicina_sira = 0
##    #pojem sir na [0,0] mestu
##    kolicina_sira += matrika_sirckov[0][0]
##    #če gremo na prvem koraku desno
##    desno = max_sircka(matrika_sirckov[:][1:])
##    #če gremo na prvem koraku dol
##    dol = max sircka(matrika_sirckov[1:][:])
##    #izberemo najboljšo pot
##    kolicina_sira += max(desno, dol)
##    return, kolicina_sira


ta

m = [[1,2,3],[4,5,6],[7,8,9]]
print('max sirckov:', max_sircka(m))
    

# Nahrbtnik
# =========
#
# Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
# poljubne izdelke, katerih skupna masa ne presega k kilogramov. (Podelili so
# tri nagrade in sicer s parametrom k = 1, k = 2 in k = 5). Napišite funkcijo
# nahrbtnik(seznam_artiklov, k), ki poišče največjo ceno, ki jo lahko odnesemo
# iz trgovine. Naredite dve verziji: pri prvi lahko vzamemo samo en artikel iste
# vrste, pri drugi pa poljubno število artiklov iste vrste.

izdelki = [
	('jogurt', 0.39, 0.18),
	('mleko', 0.89, 1.03),
    ('kava', 2.19, 0.2),
    ('maslo', 1.49, 0.25),
    ('kvas', 0.22, 0.042),
    ('jajca', 2.39, 0.69),
    ('klobasa', 3.76, 0.50),
    ('čebula', 1.29, 2.0),
    ('kruh', 2.99, 1.0),
    ('Nutella', 4.99, 0.75),
    ('sok', 1.15, 2.0)
]

from functools import lru_cache




##@memo
##def nahrbtnik_01(sez, k):
##    if sez == []:
##        return 0
##    if k <= 0:
##        return 0
##    #podatki o prvem izdelku
##    (ime, cena, teza) = sez[0]
##    #dodamo prvega
##    if teza <= k:
##        cenaZ = cena + nahrbtnik_01(sez[1:], k-teza)
##    else:
##        cenaZ = 0 + nahrbtnik_01([sez[1:], k)
##    #ce ga ne dodamo
##    cenaBrez = nahrbtnik_01(sez[1:], k)
##    return max(cenaZ, cenaBrez)


#print (nahrbtnik_01(izdelki, 3.0))    
        

# Jajca
# =====
#
# Živimo v visoki stolpnici, ki ima n nadstropij. Imamo škatlo k jajc, ki so menda zelo trpežna,
# saj naj bi prenesla padce z višjih nadstropij stoplnice. Radi bi ugotovili, katero je najvišje
# nadstopje, pri katerem jajca še preživijo padec. Ker nimamo veliko časa, bi radi poiskali
# strategijo, pri kateri bomo minimizirali število metov.
# 
# Razmislite:
#  * Kako moramo ravnati v primeru, ko imamo samo eno jajce?
#  * Kako lahko ravnamo v primeru, ko imamo na voljo zelo veliko jajc (več kot je število
#    nadstropij)?
# 
# Napišite funkcij, ki bo izračunala maksimalno število metov (v najslabšem primeru), da ugotovimo
# številko kritičnega nadstropja, če imamo na voljo točko k jajc.

#@lru_cache(maxsize = None)

def memo(f):
    izracunane= {}
    def memo_f(*args): #([1,2]) f(1,2) = f (*[1,2])
        if args not in izracunane:
            izracunane[args] = f(*args)
        return izracunane[args]
    return memo_f

zamik = 0

def naredi_glasno(f):
    def glasen_f(n, k):
        global zamik
        zamik += 2
        print(' ' * zamik, 'Računam vrednosti pri {},{}.'.format(n, k))
        y = f(n, k)
        print(' ' *  zamik, 'Izračunal sem {}'.format(y))
        zamik -= 2
        return y
    return glasen_f

@memo        
@naredi_glasno
def jajca(n, k):
    if n == 0:
        return 0
    if n == 1 and k >= 1:
        return 1
    if k == 1:
        return n
    stevila = []
    for met in range(n): #vržemo iz met-tega nadstropja
        stevilo_ce_se_razbije = 1 + jajca(met, k - 1)
        stevilo_ce_se_ne_razbije = 1 + jajca(n - met - 1, k)
        stevila.append(max(stevilo_ce_se_razbije, stevilo_ce_se_ne_razbije))
    #print(stevila)
    return min(stevila)
    
    
        
##print('2 jajci n = 100:', jajca(100, 2))
##print('3 jajca, n = 100:', jajca(100, 3))

print(jajca(10, 2))




































