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

################################################################################
################### DATA #######################################################
################################################################################


years <- "2014_2018"
#"------------------- données envoyées par ? 
dataPiedsArbres <- read.delim(paste0("Tree_bases_",years,".txt"))
#-------------------- 
dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_taxa = as.factor(TB_taxa)) %>% mutate(soil = as.factor(TB_soil.grill))
dataPiedsArbres <- dataPiedsArbres %>% mutate(STREET_number  =  TB_number..in.the.street.)
dataPiedsArbres <- dataPiedsArbres %>% mutate(site = as.factor(paste(STREET,STREET_number,sep="_")))
dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_tree = as.factor(TB_tree))
dataPiedsArbres_2014_2018 <- dataPiedsArbres %>% mutate(DATE = as.POSIXct(DATE, format = "%d/%m/%Y")) %>% mutate(YEAR = format(DATE, format="%Y")) %>%  mutate(MONTH = format(DATE, format="%m"))
dataPiedsArbres_2014_2018$site[dataPiedsArbres_2014_2018$site=='BERC_008 bis'] <- "BERC_008bis" 



### descripteurs  Sites
###############" CREATE one matrix plant - site
SitePlant  <- dataPiedsArbres_2014_2018 %>%  count(site,TB_taxa) %>% pivot_wider(names_from = site, values_from = n, values_fill = list(n = 0))
plantNames <- SitePlant$TB_taxa
SitePlant  <- SitePlant[,-1]
matSitePlant<- as.matrix(SitePlant)
colnames(matSitePlant) <- names(SitePlant)
rownames(matSitePlant) <- plantNames
w.rm <- c(which(plantNames=='(na)'),which(plantNames=='Sp.'),which(plantNames=='sp.?'))
matSitePlant <- matSitePlant[-w.rm,] # remove name plant = na
matSitePlant_2014_2018 <- t(matSitePlant)
dim(matSitePlant_2014_2018)


list_plants_2014_2018 <- colnames(matSitePlant_2014_2018)
matSitePlant_2014_2018 <- matSitePlant_2014_2018[,order(colnames(matSitePlant_2014_2018))]

#################################################" 

years <- "2009_2012"
#"------------------- données envoyées par ? 
dataPiedsArbres <- read.delim(paste0("Tree_bases_",years,".txt"))
#-------------------- 
dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_taxa = as.factor(TB_taxa)) %>% mutate(soil = as.factor(TB_soil.grill))
dataPiedsArbres <- dataPiedsArbres %>% mutate(STREET_number  =  TB_number..in.the.street.)
dataPiedsArbres <- dataPiedsArbres %>% mutate(site = as.factor(paste(STREET,STREET_number,sep="_")))
dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_tree = as.factor(TB_tree))
dataPiedsArbres_2009_2012 <- dataPiedsArbres%>% mutate(DATE = as.POSIXct(DATE, format = "%d/%m/%Y")) %>% mutate(YEAR = format(DATE, format="%Y")) %>%  mutate(MONTH = format(DATE, format="%m"))
dataPiedsArbres_2009_2012$site[dataPiedsArbres_2009_2012$site=='BERC_008 bis'] <- "BERC_008bis" 



###############" CREATE one matrix plant - site
SitePlant  <- dataPiedsArbres_2009_2012 %>%  count(site,TB_taxa) %>% pivot_wider(names_from = site, values_from = n, values_fill = list(n = 0))
plantNames <- SitePlant$TB_taxa
SitePlant  <- SitePlant[,-1]
matSitePlant<- as.matrix(SitePlant)
colnames(matSitePlant) <- names(SitePlant)
rownames(matSitePlant) <- plantNames
w.rm <- c(which(plantNames=='(na)'),which(plantNames=='Sp.'),which(plantNames=='sp.?'))
matSitePlant <- matSitePlant[-w.rm,] # remove name plant = na
matSitePlant_2009_2012 <- t(matSitePlant)
dim(matSitePlant_2009_2012)

matSitePlant_2009_2012 <- matSitePlant_2009_2012[,order(colnames(matSitePlant_2009_2012))]


################################################################### Complete Matrices
#################################################################### 

# ------------- en colonnes (plantes)
list_plants_2009_2012 <- colnames(matSitePlant_2009_2012)
list_plants_2014_2018 <- colnames(matSitePlant_2014_2018)



list_all_plants <- sort(union(list_plants_2009_2012, list_plants_2014_2018))


miss_plants_2009_2012 <- setdiff(list_all_plants, list_plants_2009_2012)
matSitePlant_2009_2012_miss <- matrix(0,nrow(matSitePlant_2009_2012),length(miss_plants_2009_2012) )
colnames(matSitePlant_2009_2012_miss) <- miss_plants_2009_2012

matSitePlant_2009_2012_complete <-cbind(matSitePlant_2009_2012, matSitePlant_2009_2012_miss)
matSitePlant_2009_2012_complete <- matSitePlant_2009_2012_complete[,order(colnames(matSitePlant_2009_2012_complete))]


miss_plants_2014_2018 <- setdiff(list_all_plants, list_plants_2014_2018)
matSitePlant_2014_2018_miss <- matrix(0,nrow(matSitePlant_2014_2018),length(miss_plants_2014_2018) )
colnames(matSitePlant_2014_2018_miss) <- miss_plants_2014_2018

matSitePlant_2014_2018_complete <-cbind(matSitePlant_2014_2018, matSitePlant_2014_2018_miss)
matSitePlant_2014_2018_complete <- matSitePlant_2014_2018_complete[,order(colnames(matSitePlant_2014_2018_complete))]

# ------------- en lignes  (sites)
list_all_sites <- sort(union(rownames(matSitePlant_2009_2012_complete), rownames(matSitePlant_2014_2018_complete)))
miss_sites_2009_2012 <- setdiff(list_all_sites, rownames(matSitePlant_2009_2012_complete))
miss_sites_2014_2018 <- setdiff(list_all_sites, rownames(matSitePlant_2014_2018_complete))

matSitePlant_2014_2018_miss_2 <- matrix(NA,length(miss_sites_2014_2018),ncol(matSitePlant_2014_2018_complete))
matSitePlant_2014_2018_complete2 <-rbind(matSitePlant_2014_2018_complete, matSitePlant_2014_2018_miss_2)
rownames(matSitePlant_2014_2018_complete2) <- c(rownames(matSitePlant_2014_2018_complete),miss_sites_2014_2018)
matSitePlant_2014_2018_complete2 <- matSitePlant_2014_2018_complete2[order(rownames(matSitePlant_2014_2018_complete2)),]

matSitePlant_2009_2012_miss_2 <- matrix(NA,length(miss_sites_2009_2012),ncol(matSitePlant_2009_2012_complete))
matSitePlant_2009_2012_complete2 <-rbind(matSitePlant_2009_2012_complete, matSitePlant_2009_2012_miss_2)
rownames(matSitePlant_2009_2012_complete2) <- c(rownames(matSitePlant_2009_2012_complete),miss_sites_2009_2012)
matSitePlant_2009_2012_complete2 <- matSitePlant_2009_2012_complete2[order(rownames(matSitePlant_2009_2012_complete2)),]


##########################################################################################


### descripteurs  Sites
# rue / grill ou pas / quel type d'arbres
descriptSites_2009_2012 <- dataPiedsArbres_2009_2012 %>%group_by(site) %>% filter(row_number() == 1) %>% select(site,STREET,soil,TB_tree) %>% arrange(site)
descriptSites_dummy_2009_2012 <- descriptSites_2009_2012  %>%  dummy_cols(select_columns = c("STREET","soil","TB_tree"))
#descriptSites_dummy_2009_2012 <- descriptSites_dummy_2009_2012 %>% select(-c("STREET","soil" ,"TB_tree"))

# rue / grill ou pas / quel type d'arbres
descriptSites_2014_2018 <- dataPiedsArbres_2014_2018 %>%group_by(site) %>% filter(row_number() == 1) %>% select(site,STREET,soil,TB_tree) %>% arrange(site)
descriptSites_dummy_2014_2018 <- descriptSites_2014_2018 %>%  dummy_cols(select_columns = c("STREET","soil","TB_tree"))
#descriptSites_dummy_2014_2018 <- descriptSites_dummy_2014_2018 %>% select(-c("STREET", "soil" ,"TB_tree"))



#-------------------- complete covar

w <- which(descriptSites_dummy_2014_2018$site %in% miss_sites_2009_2012)
descriptSites_dummy_2014_2018[w,]
descriptSites_dummy_2009_2012_complete <- rbind(descriptSites_dummy_2009_2012,descriptSites_dummy_2014_2018[w,])
descriptSites_dummy_2009_2012_complete <- descriptSites_dummy_2009_2012_complete[order(descriptSites_dummy_2009_2012_complete$site), ]



w <- which(descriptSites_dummy_2009_2012$site %in% miss_sites_2014_2018)
descriptSites_dummy_2009_2012[w,]
descriptSites_dummy_2014_2018_complete <- rbind(descriptSites_dummy_2014_2018,descriptSites_dummy_2009_2012[w,])
descriptSites_dummy_2014_2018_complete <- descriptSites_dummy_2014_2018_complete[order(descriptSites_dummy_2014_2018_complete$site), ]

nrow(descriptSites_dummy_2009_2012_complete)
nrow(descriptSites_dummy_2014_2018_complete)


descriptSites_dummy_2009_2012_complete$site[800:810]
descriptSites_dummy_2014_2018_complete$site[800:810]
u <- which(descriptSites_dummy_2009_2012_complete$TB_tree != descriptSites_dummy_2014_2018_complete$TB_tree)
descriptSites_dummy_2009_2012_complete[u,]
descriptSites_dummy_2014_2018_complete[u,]

as.character(descriptSites_dummy_2014_2018_complete$site)[descriptSites_dummy_2009_2012_complete$TB_tree != descriptSites_dummy_2014_2018_complete$TB_tree]


