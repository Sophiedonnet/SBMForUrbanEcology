import csv

##Etape 1 - Liste des pieds d'arbres observes en 2014-2018 et 2009-2012, et liste de plantes

fichier_2009_2012 = open('Tree_bases_2019_2012.csv', 'rt')
reader_2009_2012 = csv.reader(fichier_2009_2012, delimiter = '\t')

liste_pas_2009_2012 = []

liste_species_total = []

for row in reader_2009_2012:
    if row[1] != 'STREET' and row[1]+'_'+row[2] not in liste_pas_2009_2012:
        liste_pas_2009_2012.append(row[1]+'_'+row[2])
        
    if row[1] != 'STREET' and row[7] not in ['(na)', 'Sp.', 'sp.?'] and row[7] not in liste_species_total:
        liste_species_total.append(row[7])
        
fichier_2009_2012.close()

fichier_2014_2018 = open('Tree_bases_2014_2018.csv', 'rt')
reader_2014_2018 = csv.reader(fichier_2014_2018, delimiter = '\t')

liste_pas_intersect = []

for row in reader_2014_2018:
    if row[1] != 'STREET' and row[1]+'_'+row[2] not in liste_pas_intersect:
        if row[1]+'_'+row[2] in liste_pas_2009_2012:
            liste_pas_intersect.append(row[1]+'_'+row[2])
    
    if row[1] != 'STREET' and row[7] not in ['(na)', 'Sp.', 'sp.?'] and row[7] not in liste_species_total:
        liste_species_total.append(row[7])

liste_pas_intersect.sort()
liste_species_total.sort()

#Etape 2 - Creation des matrices d'observation
matrice_2009_2012 = [[0 for plante in liste_species_total] for pa in liste_pas_intersect]
matrice_2014_2018 = [[0 for plante in liste_species_total] for pa in liste_pas_intersect]

fichier_2009_2012 = open('Tree_bases_2019_2012.csv', 'rt')
reader_2009_2012 = csv.reader(fichier_2009_2012, delimiter = '\t')

for row in reader_2009_2012:
    nom_pa = row[1]+'_'+row[2]
    nom_plante = row[7]
    
    if nom_pa in liste_pas_intersect and nom_plante in liste_species_total:
        matrice_2009_2012[liste_pas_intersect.index(nom_pa)][liste_species_total.index(nom_plante)] = 1
        
fichier_2009_2012.close()
            
fichier_2014_2018 = open('Tree_bases_2014_2018.csv', 'rt')
reader_2014_2018 = csv.reader(fichier_2014_2018, delimiter = '\t')

for row in reader_2014_2018:
    nom_pa = row[1]+'_'+row[2]
    nom_plante = row[7]
    
    if nom_pa in liste_pas_intersect and nom_plante in liste_species_total:
        matrice_2014_2018[liste_pas_intersect.index(nom_pa)][liste_species_total.index(nom_plante)] = 1
        
fichier_2014_2018.close()

#Etape 3 - Creation des fichiers texte
with open('network_2009_2012.txt', 'w') as f:
    f.write('STREET\t')
    for species in liste_species_total[0:len(liste_species_total)-1]:
        f.write(species+'\t')
    f.write(liste_species_total[-1]+'t')
    f.write('\n')

    for pa in liste_pas_intersect:
        f.write(pa+'\t')
        for obs in matrice_2009_2012[liste_pas_intersect.index(pa)][0:len(liste_species_total)-1]:
            f.write(str(obs)+'\t')
        f.write(str(matrice_2009_2012[liste_pas_intersect.index(pa)][-1]))
        f.write('\n')
        
with open('network_2014_2018.txt', 'w') as f:
    f.write('STREET\t')
    for species in liste_species_total[0:len(liste_species_total)-1]:
        f.write(species+'\t')
    f.write(liste_species_total[-1]+'t')
    f.write('\n')

    for pa in liste_pas_intersect:
        f.write(pa+'\t')
        for obs in matrice_2014_2018[liste_pas_intersect.index(pa)][0:len(liste_species_total)-1]:
            f.write(str(obs)+'\t')
        f.write(str(matrice_2014_2018[liste_pas_intersect.index(pa)][-1]))
        f.write('\n')


