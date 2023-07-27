rm(list=ls())
library(dplyr)
library(tidyr)
library(fastDummies)
library(ggplot2)
library(sbm)
library(reshape2)
library(forcats)
library(gtools)
source('functions_PiedsArbres.R')
#------------------------------------------------------------------- 

###########################  Functions
modifDataPiedArbres = function(dataPiedsArbres){
  dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_taxa = as.factor(TB_taxa)) %>% mutate(soil = as.factor(TB_soil.grill))
  dataPiedsArbres <- dataPiedsArbres %>% mutate(STREET_number  =  TB_number..in.the.street.)
  dataPiedsArbres <- dataPiedsArbres %>% mutate(site = as.factor(paste(STREET,STREET_number,sep="_")))
  dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_tree = as.factor(TB_tree))
  dataPiedsArbres <- dataPiedsArbres %>% mutate(DATE = as.POSIXct(DATE, format = "%d/%m/%Y")) %>% mutate(YEAR = format(DATE, format="%Y")) %>%  mutate(MONTH = format(DATE, format="%m"))
  dataPiedsArbres$site[dataPiedsArbres$site=='BERC_008 bis'] <- "BERC_008bis" 
  return(dataPiedsArbres)
}

buildMatrixSitePlants  = function(dataPiedsArbres){
  SitePlant  <- dataPiedsArbres %>%  count(site,TB_taxa) %>% pivot_wider(names_from = site, values_from = n, values_fill = list(n = 0))
  plantNames <- SitePlant$TB_taxa
  SitePlant  <- SitePlant[,-1]
  matSitePlant<- t(as.matrix(SitePlant))
  colnames(matSitePlant) <- plantNames
  o <- order(colnames(matSitePlant))
  matSitePlant <- matSitePlant[,o]
  
  #------------------- 
  w.rm <- c(which(plantNames[o]=='(na)'),which(plantNames[o]=='Sp.'),which(plantNames[o]=='sp.?'))
  if (length(w.rm)>1){
    na.column <- rowSums(matSitePlant[,w.rm]) 
  }
  if(length(w.rm)==1){
    na.column <- matSitePlant[,w.rm] 
  }
  matSitePlant <- matSitePlant[,-w.rm] # remove name plant = na
  matSitePlant <- cbind(matSitePlant, na.column) # add the unique na column
  colnames(matSitePlant)[ncol(matSitePlant)] = '(na)'
  return(matSitePlant)
}
################################################################################
################### DATA #######################################################
################################################################################


years <- "2014_2018"
dataPiedsArbres_2014_2018 <- modifDataPiedArbres(read.delim(paste0("Tree_bases_",years,".txt")))
matSitePlant_2014_2018 <- buildMatrixSitePlants(dataPiedsArbres_2014_2018)

#-------------------- 
years <- "2009_2012"
dataPiedsArbres_2009_2012 <- modifDataPiedArbres(read.delim(paste0("Tree_bases_",years,".txt")))
matSitePlant_2009_2012 <- buildMatrixSitePlants(dataPiedsArbres_2009_2012)





################################################################### Complete Matrices
#################################################################### 

# ------------- en colonnes (plantes)
list_plants_2009_2012 <- colnames(matSitePlant_2009_2012)
list_plants_2014_2018 <- colnames(matSitePlant_2014_2018)
list_all_plants <- union(list_plants_2009_2012, list_plants_2014_2018)


miss_plants_2009_2012 <- setdiff(list_all_plants, list_plants_2009_2012)
length(miss_plants_2009_2012)
matSitePlant_2009_2012_miss <- matrix(0,nrow(matSitePlant_2009_2012),length(miss_plants_2009_2012) )
colnames(matSitePlant_2009_2012_miss) <- miss_plants_2009_2012
matSitePlant_2009_2012_complete <-cbind(matSitePlant_2009_2012, matSitePlant_2009_2012_miss)
matSitePlant_2009_2012_complete <- matSitePlant_2009_2012_complete[,order(colnames(matSitePlant_2009_2012_complete))]


miss_plants_2014_2018 <- setdiff(list_all_plants, list_plants_2014_2018)
matSitePlant_2014_2018_miss <- matrix(0,nrow(matSitePlant_2014_2018),length(miss_plants_2014_2018) )
colnames(matSitePlant_2014_2018_miss) <- miss_plants_2014_2018
matSitePlant_2014_2018_complete <-cbind(matSitePlant_2014_2018, matSitePlant_2014_2018_miss)
matSitePlant_2014_2018_complete <- matSitePlant_2014_2018_complete[,order(colnames(matSitePlant_2014_2018_complete))]

# ------------- en lignes  (sites) ! remove sites which are not observed on both period. 
list_all_sites <- sort(union(rownames(matSitePlant_2009_2012_complete), rownames(matSitePlant_2014_2018_complete)))
miss_sites_2009_2012 <- setdiff(list_all_sites, rownames(matSitePlant_2009_2012_complete))
miss_sites_2014_2018 <- setdiff(list_all_sites, rownames(matSitePlant_2014_2018_complete))
miss_all_sites <- union(miss_sites_2014_2018,miss_sites_2009_2012)


W1 <- which(rownames(matSitePlant_2014_2018_complete) %in% miss_all_sites)
matSitePlant_2014_2018_complete2 <-matSitePlant_2014_2018_complete[-W1, ]
dim(matSitePlant_2014_2018_complete2)

W2 <- which(rownames(matSitePlant_2009_2012_complete) %in% miss_all_sites)
matSitePlant_2009_2012_complete2 <-matSitePlant_2009_2012_complete[-W2, ]
dim(matSitePlant_2009_2012_complete2)


##########################################################################################


### descripteurs  Sites
# rue / grill ou pas / quel type d'arbres
descriptSites_2009_2012 <- dataPiedsArbres_2009_2012 %>%group_by(site) %>% filter(row_number() == 1) %>% select(site,STREET,STREET_number,soil,TB_tree) %>% arrange(site)
descriptSites_dummy_2009_2012 <- descriptSites_2009_2012  %>%  dummy_cols(select_columns = c("STREET","soil","TB_tree"))

 
descriptSites_2014_2018 <- dataPiedsArbres_2014_2018 %>%group_by(site) %>% filter(row_number() == 1) %>% select(site,STREET,STREET_number,soil,TB_tree) %>% arrange(site)
descriptSites_dummy_2014_2018 <- descriptSites_2014_2018 %>%  dummy_cols(select_columns = c("STREET","soil","TB_tree"))



#-------------------- remove covar dfor site not observed both years

w1 <- which(as.character(descriptSites_dummy_2014_2018$site) %in% miss_all_sites)
descriptSites_dummy_2014_2018_complete <- descriptSites_dummy_2014_2018[-w1,]
 
w2 <- which(as.character(descriptSites_dummy_2009_2012$site) %in% miss_all_sites)
descriptSites_dummy_2009_2012_complete <- descriptSites_dummy_2009_2012[-w2,]

nrow(descriptSites_dummy_2009_2012_complete)
nrow(descriptSites_dummy_2014_2018_complete)


u = sample(1:1310,1)
descriptSites_dummy_2009_2012_complete$site[u:(u+10)]
descriptSites_dummy_2014_2018_complete$site[u:(u+10)]
u <- which(descriptSites_dummy_2009_2012_complete$TB_tree != descriptSites_dummy_2014_2018_complete$TB_tree)
u <- which(descriptSites_dummy_2009_2012_complete$soil != descriptSites_dummy_2014_2018_complete$soil)
length(u)


descriptSites_dummy_2009_2012_complete[u,]
descriptSites_dummy_2014_2018_complete[u,]


###################################" Final save 
matSitePlant_2009_2012 <- matSitePlant_2009_2012_complete2
matSitePlant_2014_2018 <- matSitePlant_2014_2018_complete2
descriptSites_dummy_2009_2012 <- descriptSites_dummy_2009_2012_complete   %>% select(-c("STREET", "soil" ,"TB_tree"))
descriptSites_dummy_2014_2018 <- descriptSites_dummy_2014_2018_complete   %>% select(-c("STREET", "soil" ,"TB_tree"))

save(matSitePlant_2014_2018 , descriptSites_dummy_2014_2018 , file = 'dataForStateOfTheR/dataPiedsArbres_2014_2018.Rdata')
save(matSitePlant_2009_2012 , descriptSites_dummy_2009_2012 , file = 'dataForStateOfTheR/dataPiedsArbres_2009_2012.Rdata')
################ Add position

# Coordonnees_PA <- read.csv("~/WORK_LOCAL/RECHERCHE/TRAVAUX_DE_RECHERCHE/Louvet-Appoline/SBMForUrbanEcology/dataAppoline/Coordonnees_PA.csv", sep=";")
# Coordonnees_PA <- Coordonnees_PA %>% mutate(site = as.factor(paste(PA_voie,PA_numérobis,sep="_")))
# Coordonnees_PA <- Coordonnees_PA[which(Coordonnees_PA$site %in% as.character(descriptSites_dummy_2009_2012$site)), ] 
# Coordonnees_PA <- Coordonnees_PA[order(as.character(Coordonnees_PA$site)), ]
# 
# descriptSites_dummy_2009_2012 <- descriptSites_dummy_2009_2012 %>% mutate(COORD_X  = Coordonnees_PA$COORD_X)
# descriptSites_dummy_2009_2012 <- descriptSites_dummy_2009_2012 %>% mutate(COORD_Y  = Coordonnees_PA$COORD_Y)
# 
# descriptSites_dummy_2014_2018<- descriptSites_dummy_2014_2018 %>% mutate(COORD_X  = Coordonnees_PA$COORD_X)
# descriptSites_dummy_2014_2018 <- descriptSites_dummy_2014_2018 %>% mutate(COORD_Y  = Coordonnees_PA$COORD_Y)
# 
# 
# save(matSitePlant_2014_2018 , descriptSites_dummy_2014_2018 , file = 'dataForEmre/dataPiedsArbres_2014_2018.Rdata')
# save(matSitePlant_2009_2012 , descriptSites_dummy_2009_2012 , file = 'dataForEmre/dataPiedsArbres_2009_2012.Rdata')

dim(matSitePlant_2009_2012)
dim(matSitePlant_2014_2018)
dim(descriptSites_dummy_2014_2018)
dim(descriptSites_dummy_2009_2012)


############################## 
load('dataForStateOfTheR/dataPiedsArbres_2014_2018.Rdata')

