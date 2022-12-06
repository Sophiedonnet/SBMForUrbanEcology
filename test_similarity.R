rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(sbm)
library(reshape2)
source('functions_PiedsArbres.R')


#"------------------- Chargement des donnees
dataSimilarity_2014_2018 <- read.delim("similarity_tree_bases.txt", header = FALSE)
covarSpecies5 <- read.delim('covar_Sorbus sp. ?.txt', header = FALSE)
listeRues <- covarSpecies5$V1
matSimilarity <- as.matrix(dataSimilarity_2014_2018)
colnames(matSimilarity) <- listeRues
rownames(matSimilarity) <- listeRues

#Test 1 - Sans prendre en compte la distance entre les patchs
SBM_no_covar <- matSimilarity %>% 
  estimateSimpleSBM("poisson", dimLabels = "Pied d'arbre")
save(SBM_no_covar,file='resSBM_similarity_no_covar.Rdata')
plot(mySimpleSBMPoisson, type = "expected", dimLabels =c("Pied d'arbre"))
