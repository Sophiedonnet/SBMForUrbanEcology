rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(sbm)
library(reshape2)
source('functions_PiedsArbres.R')


#"------------------- Chargement des donnees
dataDistance <- read.delim("distance_pas.txt", header = FALSE)
listeRues <- dataDistance$V1
dataDistance <- dataDistance[,-1]
matDistance <- as.matrix(dataDistance)
colnames(matDistance) <- listeRues
rownames(matDistance) <- listeRues

dataSimilarity_2014_2018 <- read.delim("similarity_tree_bases.txt", header = FALSE)
matSimilarity <- as.matrix(dataSimilarity_2014_2018)
colnames(matSimilarity) <- listeRues
rownames(matSimilarity) <- listeRues

#Test 1 - Sans prendre en compte la distance entre les patchs
SBM_covar <- matSimilarity %>% 
  estimateSimpleSBM("poisson", dimLabels = "Pied d'arbre", covariates = list(matSimilarity))
save(SBM_covar,file='resSBM_similarity_covar.Rdata')
plot(mySimpleSBMPoisson, type = "expected", dimLabels =c("Pied d'arbre"))
