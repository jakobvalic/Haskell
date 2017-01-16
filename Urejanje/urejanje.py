"""
Implementacije algoritmov za urejanje
"""

def naivno_uredi(l):
    for i in range(0, len(l) - 1):
        menjava = False
        for j in range(0, len(l) - i - 1):
            if l[j] > l[j + 1]:
                l[j], l[j + 1] = l[j + 1], l[j]  # swap
            menjava = True
        if menjava == False:
            break
    return l

def vgrajeni_sort(l):
    l.sort()

def razdeli(l):
    pivot = l[0]
    manjsi = []
    vecji = []
    for x in l[1:]:
        if x < pivot:
            manjsi.append(x)
        else:
            vecji.append(x)
    return manjsi, pivot, vecji

def hitro_uredi_z_novimi_seznami(l):
    '''Porabi toliko prostora,kot je velik l.'''
    if len(l) <= 1:
        return l
    manjsi, pivot, vecji= razdeli(l)
    manjsi_urejen = hitro_uredi_z_novimi_seznami(manjsi)
    vecji_urejen = hitro_uredi_z_novimi_seznami(vecji)
    return manjsi_urejen + [pivot] + vecji_urejen

def razdeli_na_mestu(l, spodnja_meja, zgornja_meja):
    '''Vzame sez in ga preuredi na mestu.'''   
    pivot = l[zgornja_meja]
    v = spodnja_meja - 1 # vijugasta puščica, povečamo jo pred menjavo
    for i in range(spodnja_meja, zgornja_meja + 1): # i = navadna puščica, + 1 poskrbi, da se zamenja tudi pivot
        #print(l)
        if l[i] <= pivot: # 
            v += 1 # premaknemo vijugasto puščico
            l[v], l[i] = l[i], l[v] # naredimo menjavo
    return v #vrnemo indeks pivota, ki je na mestu


    #ALTERNATIVA:
##    v += 1
##    l[zgornja_meja], l[v] = l[v], l[zgornja_meja]
    


            
 
def hitro_uredi_na_mestu(l, spodnja_meja, zgornja_meja):
    if spodnja_meja >= zgornja_meja:
        return 
    v = razdeli_na_mestu(l, spodnja_meja, zgornja_meja)
    hitro_uredi_na_mestu(l, spodnja_meja, v - 1)
    hitro_uredi_na_mestu(l, v + 1, zgornja_meja)
    return l
    

def hitro_uredi(l):
    hitro_uredi_na_mestu(l, 0, len(l) - 1)
    return l
    
        
def uredi_z_zlivanjem(l):
    pass  # TODO


#sez = [0,1,2,3,-1,42,-4]
#hitro_uredi_na_mestu(sez)


#import random
sez = [2, 1, 3, 5, 6, 7, 8, 4, 2]
#sez = random.shuffle(sez)
print(hitro_uredi_na_mestu(sez, 0, len(sez) - 1))
print(hitro_uredi_na_mestu(sez, 0, len(sez) - 1))
print(def hitro_uredi_z_novimi_seznami(l))












