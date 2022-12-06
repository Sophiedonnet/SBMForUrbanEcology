rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(sbm)
library(reshape2)
source('functions_PiedsArbres.R')

#Chargement des donnees - Periode 2014-2018
data2014_2018 = read.delim('network_2014_2018.txt', header = TRUE)
siteNames <- data2014_2018$STREET
data2014_2018 <- data2014_2018[,-1]
mat2014_2018 <- as.matrix(data2014_2018)
colnames(mat2014_2018) <- names(data2014_2018)
rownames(mat2014_2018) <- siteNames

dim(mat2014_2018)

plotMyMatrix(mat2014_2018,dimLabels =  c('Sites','Plants'))

#Chargement des donnees - Periode 2009-2012
data2009_2012 = read.delim('network_2009_2012.txt', header = TRUE)
siteNames <- data2009_2012$STREET
data2009_2012 <- data2009_2012[,-1]
mat2009_2012 <- as.matrix(data2009_2012)
colnames(mat2009_2012) <- names(data2009_2012)
rownames(mat2009_2012) <- siteNames

dim(mat2009_2012)

plotMyMatrix(mat2009_2012,dimLabels =  c('Sites','Plants'))

sbm2014_2018 = defineSBM(mat2014_2018,model='bernoulli',dimLabels = c("Pieds d'arbre", "Espece"))
sbm2009_2012 = defineSBM(mat2009_2012,model='bernoulli',dimLabels = c("Pieds d'arbre", "Espece"))

listMultiplex = list(sbm2009_2012,sbm2014_2018)
plotMyMultiplexMatrix(listMultiplex)

res = estimateMultiplexSBM(listMultiplex)
res$storedModels
plot(res)
plot(res, type = 'expected')
save(res, file = 'multiplexIndep.RData')

resDep = estimateMultiplexSBM(listMultiplex, dependent = TRUE)
resDep$storedModels
plot(resDep, type = 'expected')
plot(resDep)

save(resDep, file = 'multiplexDep.RData')


