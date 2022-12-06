rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(sbm)
library(reshape2)
source('functions_PiedsArbres.R')


#"------------------- données envoyées par ? 
dataPiedsArbres_2014_2018 <- read.delim("Tree_bases_2014_2018.txt")

#-------------------- 
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(TB_taxa = as.factor(TB_taxa)) %>% mutate(soil = as.factor(TB_soil.grill))
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(STREET_number  =  TB_number..in.the.street.)
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(site = as.factor(paste(STREET,STREET_number,sep="_")))
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(TB_tree = as.factor(TB_tree))
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(DATE = as.POSIXct(DATE, format = "%d/%m/%Y")) %>% mutate(YEAR = format(DATE, format="%Y")) %>%  mutate(MONTH = format(DATE, format="%m"))

SitePlant_2014_2018  <- dataPiedsArbres_2014_2018 %>%  count(site,TB_taxa) %>% pivot_wider(names_from = site, values_from = n, values_fill = list(n = 0))
plantNames_2014_2018 <- SitePlant_2014_2018$TB_taxa
SitePlant_2014_2018  <- SitePlant_2014_2018[,-1]
matSitePlant_2014_2018 <- as.matrix(SitePlant_2014_2018)
colnames(matSitePlant_2014_2018) <- names(SitePlant_2014_2018)
rownames(matSitePlant_2014_2018) <- plantNames_2014_2018

w.rm <- c(which(plantNames_2014_2018=='(na)'),which(plantNames_2014_2018=='Sp.'),which(plantNames_2014_2018=='sp.?'))

matSitePlant_2014_2018 <- matSitePlant_2014_2018[-w.rm,] # remove name plant = na
matSitePlant_2014_2018 <- t(matSitePlant_2014_2018)
dim(matSitePlant_2014_2018)

#Attention : il faut d'abord ordonner les csvs de covariables de façon à ce que 
#les pieds d'arbres soient listés dans le même ordre que dans matSitePlant

covarGrille <- read.delim('covar_grid.txt', header = FALSE)
listeRues <- covarGrille$V1
covarGrille <- covarGrille[,-1]
matCovarGrille <- as.matrix(covarGrille)
colnames(matCovarGrille) <- colnames(matSitePlant_2014_2018)
rownames(matCovarGrille) <- listeRues

#Creation d'une matrice par espece
covarSpecies1 <- read.delim('covar_Aesculus hippocastanum.txt', header = FALSE)
listeRues <- covarSpecies1$V1
covarSpecies1 <- covarSpecies1[,-1]
matCovarSpecies1 <- as.matrix(covarSpecies1)
colnames(matCovarSpecies1) <- colnames(matSitePlant_2014_2018)
rownames(matCovarSpecies1) <- listeRues

covarSpecies2 <- read.delim('covar_Melia azedarach.txt', header = FALSE)
listeRues <- covarSpecies2$V1
covarSpecies2 <- covarSpecies2[,-1]
matCovarSpecies2 <- as.matrix(covarSpecies2)
colnames(matCovarSpecies2) <- colnames(matSitePlant_2014_2018)
rownames(matCovarSpecies2) <- listeRues

covarSpecies3 <- read.delim('covar_Platanus x acerifolia.txt', header = FALSE)
listeRues <- covarSpecies3$V1
covarSpecies3 <- covarSpecies3[,-1]
matCovarSpecies3 <- as.matrix(covarSpecies3)
colnames(matCovarSpecies3) <- colnames(matSitePlant_2014_2018)
rownames(matCovarSpecies3) <- listeRues

covarSpecies4 <- read.delim('covar_Prunus sp..txt', header = FALSE)
listeRues <- covarSpecies4$V1
covarSpecies4 <- covarSpecies4[,-1]
matCovarSpecies4 <- as.matrix(covarSpecies4)
colnames(matCovarSpecies4) <- colnames(matSitePlant_2014_2018)
rownames(matCovarSpecies4) <- listeRues

covarSpecies5 <- read.delim('covar_Sorbus sp. ?.txt', header = FALSE)
listeRues <- covarSpecies5$V1
covarSpecies5 <- covarSpecies5[,-1]
matCovarSpecies5 <- as.matrix(covarSpecies5)
colnames(matCovarSpecies5) <- colnames(matSitePlant_2014_2018)
rownames(matCovarSpecies5) <- listeRues

covarSpecies6 <- read.delim('covar_Tilia sp..txt', header = FALSE)
listeRues <- covarSpecies6$V1
covarSpecies6 <- covarSpecies6[,-1]
matCovarSpecies6 <- as.matrix(covarSpecies6)
colnames(matCovarSpecies6) <- colnames(matSitePlant_2014_2018)
rownames(matCovarSpecies6) <- listeRues

listCovar = list(matCovarGrille, matCovarSpecies1, matCovarSpecies2, matCovarSpecies3, matCovarSpecies4, matCovarSpecies5, matCovarSpecies6)

#------------- Binary 
matSitePlant_bin_2014_2018 <- 1*(matSitePlant_2014_2018>0)
plotMyMatrix(matSitePlant_bin_2014_2018,dimLabels =  c('Sites','Plants'))

hist(rowSums(matSitePlant_bin_2014_2018))
hist(colSums(matSitePlant_bin_2014_2018))

mySBM_bern_2014_2018 <- estimateBipartiteSBM(matSitePlant_bin_2014_2018,dimLabels = c('Sites','Plants'), covariates = listCovar)
save(mySBM_bern_2014_2018,file='resSBM_Bern_2014_2018_covar.Rdata')
plot(mySBM_bern_2014_2018,type = c("expected"),plotOptions = list(line.width = 1/10))
