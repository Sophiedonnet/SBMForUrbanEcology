rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(sbm)
library(reshape2)
source('functions_PiedsArbres.R')



#--------------------  Le fichier Sites plantes

load('res/resSBM_Pois_2014_2018.Rdata')
matSitePlant_2014_2018 <- mySBM_pois_2014_2018$networkData
nbSites <- nrow(matSitePlant_2014_2018)
#---------------------- Grille 

#Attention : il faut d'abord ordonner les csvs de covariables de façon à ce que 
#les pieds d'arbres soient listés dans le même ordre que dans matSitePlant

covarGrille <- read.delim('dataAppoline/covar_grid.txt', header = FALSE,row.names = 1)
matCovarGrille <- as.matrix(covarGrille)
colnames(matCovarGrille) <- colnames(matSitePlant_2014_2018)

Grille <- as.factor(matCovarGrille[,1])
table(Grille,mySBM_pois_2014_2018$memberships$Sites)



#------------------------------ Espèce d'arbre
TreeSpecies <- rep(0,nbSites)

names(TreeSpecies) <- as.vector(U[,1])
#Creation d'une matrice par espèce
covarSpecies1 <- read.delim('dataAppoline/covar_Aesculus hippocastanum.txt', header = FALSE,row.names = 1)
matCovarSpecies1 <- as.matrix(covarSpecies1)
colnames(matCovarSpecies1) <- colnames(matSitePlant_2014_2018)


covarSpecies2 <- read.delim('dataAppoline/covar_Melia azedarach.txt', header = FALSE,row.names = 1)
matCovarSpecies2 <- as.matrix(covarSpecies2)
colnames(matCovarSpecies2) <- colnames(matSitePlant_2014_2018)

covarSpecies3 <- read.delim('dataAppoline/covar_Platanus x acerifolia.txt', header = FALSE,row.names = 1)
matCovarSpecies2 <- as.matrix(covarSpecies2)
colnames(matCovarSpecies3) <- colnames(matSitePlant_2014_2018)

covarSpecies4 <- read.delim('dataAppoline/covar_Prunus sp..txt', header = FALSE, row.names = 1)
matCovarSpecies4 <- as.matrix(covarSpecies4)
colnames(matCovarSpecies4) <- colnames(matSitePlant_2014_2018)

covarSpecies5 <- read.delim('dataAppoline/covar_Sorbus sp. ?.txt', header = FALSE, row.names = 1)
matCovarSpecies5 <- as.matrix(covarSpecies5)
colnames(matCovarSpecies5) <- colnames(matSitePlant_2014_2018)

covarSpecies6 <- read.delim('dataAppoline/covar_Tilia sp..txt', header = FALSE, row.names = 1)
matCovarSpecies6 <- as.matrix(covarSpecies6)
colnames(matCovarSpecies6) <- colnames(matSitePlant_2014_2018)

listCovar = list(matCovarGrille, matCovarSpecies1, matCovarSpecies2, matCovarSpecies3, matCovarSpecies4, matCovarSpecies5, matCovarSpecies6)

#------------- Binary 
matSitePlant_bin_2014_2018 <- 1*(matSitePlant_2014_2018>0)
plotMyMatrix(matSitePlant_bin_2014_2018,dimLabels =  c('Sites','Plants'))

hist(rowSums(matSitePlant_bin_2014_2018))
hist(colSums(matSitePlant_bin_2014_2018))

mySBM_bern_2014_2018 <- estimateBipartiteSBM(matSitePlant_bin_2014_2018,dimLabels = c('Sites','Plants'), covariates = listCovar)
save(mySBM_bern_2014_2018,file='resSBM_Bern_2014_2018_covar.Rdata')
plot(mySBM_bern_2014_2018,type = c("expected"),plotOptions = list(line.width = 1/10))
