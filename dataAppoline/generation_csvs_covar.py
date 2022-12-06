import csv

#Etape 1 - Creation de la matrice de covariable pour la presence/absence de grille

fichier = open('char_pa.csv', 'rt')
reader = csv.reader(fichier)

liste_pas = []
liste_covar = []

for row in reader:
    if row[0] != 'STREET' and row[0]+'_'+row[1] != 'MONT_53' and row[0]+'_'+row[1] not in liste_pas:
        liste_pas.append(row[0]+'_'+row[1])
        if row[3] == 'soil':
            liste_covar.append('0')
        else:
            liste_covar.append('1')

fichier.close()

fichier = open('Tree_bases_2014_2018.csv', 'rt')
reader = csv.reader(fichier, delimiter = '\t')

liste_pas_bis = []

for row in reader:
    if row[1]+'_'+row[2] != 'STREET' and row[1]+'_'+row[2] not in liste_pas_bis:
        liste_pas_bis.append(row[1]+'_'+row[2])
fichier.close()

for pa in liste_pas:
    if pa not in liste_pas_bis:
        print(pa)

liste_pas_sorted = liste_pas[:]
liste_pas_sorted.sort()

with open('covar_grid.txt', 'w') as f:
    for pa in liste_pas_sorted:
        f.write(pa+'\t')
        for i in range(252):
            f.write(liste_covar[liste_pas.index(pa)]+'\t')
        f.write(liste_covar[liste_pas.index(pa)])
        f.write('\n')
    
#Etape 2 - Creation de la matrice de covariables pour chacune des especes de pied d'arbre possible
liste_especes_pa = ['Platanus x acerifolia', 'Aesculus hippocastanum', 'Melia azedarach', 'Tilia sp.', 'Sorbus sp. ?', 'Prunus sp.']    

liste_pas = []
liste_covar = [[] for espece in liste_especes_pa]

fichier = open('char_pa.csv', 'rt')
reader = csv.reader(fichier)

for row in reader:
    if row[0] != 'STREET' and row[0]+'_'+row[1] != 'MONT_53' and row[0]+'_'+row[1] not in liste_pas:
        liste_pas.append(row[0]+'_'+row[1])
        for i in range(len(liste_especes_pa)):
            if row[2] == liste_especes_pa[i]:
                liste_covar[i].append('1')
            else:
                liste_covar[i].append('0')

fichier.close()

liste_pas_sorted = liste_pas[:]
liste_pas_sorted.sort()

for i in range(len(liste_especes_pa)):
    with open('covar_'+liste_especes_pa[i]+'.txt', 'w') as f:
        for pa in liste_pas_sorted:
            f.write(pa+'\t')
            for j in range(252):
                f.write(liste_covar[i][liste_pas.index(pa)]+'\t')
            f.write(liste_covar[i][liste_pas.index(pa)])
            f.write('\n')
