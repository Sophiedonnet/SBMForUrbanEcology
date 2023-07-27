rm(list=ls())
library(mvabund)
library(gllvm)
load(file = 'dataForEmre/dataPiedsArbres_2009_2012.Rdata')



Y <- matSitePlant_2009_2012
Y <- as.matrix(Y)

spe <- order(colSums(Y), decreasing = TRUE)[1:20]
Y <- Y[,spe]

X <- as.data.frame(descriptSites_dummy_2009_2012[, -1])
rownames(X) <- descriptSites_dummy_2009_2012[,1]$site
colnames(X)[17] <- 'soil_na'
colnames(X)[23] <- 'TB_tree_Prunus'
colnames(X)[24] <- 'TB_tree_Sorbus'
colnames(X)
is.matrix(X)



#res_GLLVM  = gllvm(y = Y, X = X, family = 'poisson')


library(PLNmodels)
data_PLN <- prepare_data(counts = Y, covariates = X)
res_PLNPCA <- PLNPCA(Abundance ~ 1 + COORD_X + COORD_Y, data=data_PLN, ranks = 1:5  )

