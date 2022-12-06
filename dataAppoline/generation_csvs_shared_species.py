import csv
import numpy as np

#Etape 1 - Creation de la liste des PAs pour 2014-2018

fichier = open('char_pa.csv', 'rt')
reader = csv.reader(fichier)

liste_pas = []

for row in reader:
    if row[0] != 'STREET' and row[0]+'_'+row[1] not in ['MONT_53', 'BERC_008 bis', 'RBER_137', 'RBER_138'] and row[0]+'_'+row[1] not in liste_pas:
        liste_pas.append(row[0]+'_'+row[1])

fichier.close()

liste_pas.sort()

#Etape 2 - Creation de la liste des especes observees dans chaque PA
liste_especes_pas = [[] for pa in liste_pas]

fichier = open('Tree_bases_2014_2018.csv', 'rt')
reader = csv.reader(fichier, delimiter = '\t')

for row in reader:
    if row[1] != 'STREET' and row[7] not in ['(na)', 'Sp.', 'sp.?']:
        nom_pa = row[1]+'_'+row[2]
        if nom_pa in liste_pas:
            liste_especes_pas[liste_pas.index(nom_pa)].append(row[7])
        
fichier.close()

#Etape 3 - Comptage du nombre d'especes communes
liste_nbr_especes_communes = [[0 for pa in liste_pas] for pa2 in liste_pas]

for ind_pa_row in range(len(liste_pas)):
    for ind_pa_col in range(len(liste_pas)):
        if ind_pa_row != ind_pa_col:
            liste_nbr_especes_communes[ind_pa_row][ind_pa_col] = len(list(set(liste_especes_pas[ind_pa_row]).intersection(liste_especes_pas[ind_pa_col])))
            
#Etape 4 - Creation du fichier avec les resultats
with open('similarity_tree_bases.txt', 'w') as f:
    for row in liste_nbr_especes_communes:
        for elem in row[0:len(row)-1]:
            f.write(str(elem)+('\t'))
        f.write(str(row[-1]))
        f.write('\n')
       
#Etape 5 - Recuperation des coordonnees de chaque PA
liste_coord_pa = [[] for pa in liste_pas]

fichier = open('Coordonnees_PA.csv', 'rt')
reader = csv.reader(fichier, delimiter = ';')

for row in reader:
    if row[0] != 'PA_voie' and row[0]+'_'+row[1] in liste_pas:
        liste_coord_pa[liste_pas.index(row[0]+'_'+row[1])] = [float(row[2]), float(row[3])]

fichier.close()

#Test pour voir si j'ai bien recupere toutes les coordonnees
for i in range(len(liste_coord_pa)):
    if liste_coord_pa[i] == []:
        print(liste_pas[i])
        
#Etape 6 - Calcul des distances entre PAs
liste_distance = [[0 for pa in liste_pas] for pa2 in liste_pas]

for pa1 in range(len(liste_pas)):
    for pa2 in range(len(liste_pas)):
        coord_pa1 = liste_coord_pa[pa1]
        coord_pa2 = liste_coord_pa[pa2]
        
        diff_x = (coord_pa1[0]-coord_pa2[0])**2
        diff_y = (coord_pa1[1]-coord_pa2[1])**2
        
        dist = np.sqrt(diff_x+diff_y)
        
        liste_distance[pa1][pa2] = dist
        
with open('distance_pas.txt', 'w') as f:
    for i in range(len(liste_distance)): 
        row = liste_distance[i]
        f.write(liste_pas[i]+('\t'))
        
        for elem in row[0:len(row)-1]:
            f.write(str(elem)+('\t'))
        f.write(str(row[-1]))
        f.write('\n')


        
        
