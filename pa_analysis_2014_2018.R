rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(sbm)
library(reshape2)
library(forcats)
source('functions_PiedsArbres.R')


#"------------------- données envoyées par ? 
dataPiedsArbres_2014_2018 <- read.delim("Tree_bases_2014_2018.txt")
#-------------------- 
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(TB_taxa = as.factor(TB_taxa)) %>% mutate(soil = as.factor(TB_soil.grill))
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(STREET_number  =  TB_number..in.the.street.)
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(site = as.factor(paste(STREET,STREET_number,sep="_")))
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(TB_tree = as.factor(TB_tree))
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(DATE = as.POSIXct(DATE, format = "%d/%m/%Y")) %>% mutate(YEAR = format(DATE, format="%Y")) %>%  mutate(MONTH = format(DATE, format="%m"))
#dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(TB_taxa = fct_collapse(A = c("A","B")))

################### QUICK EXPLORATION
#- Types d'arbres  
dataPiedsArbres_2014_2018%>% group_by(site,TB_tree) %>%summarise(n = n(),.groups = 'drop') %>% ggplot(aes(x=TB_tree))+geom_bar() +coord_flip() +  scale_fill_brewer(palette='Greens') + ggtitle("Tree Species")

# Relevés par années
dataPiedsArbres_2014_2018%>% group_by(site,YEAR) %>%summarise(n = 1*(n()>1),.groups = 'drop') %>% ggplot(aes(x=YEAR))+geom_bar() +coord_flip() +  scale_fill_brewer(palette='Greens') + ggtitle("Nb of sites sampled  at least on time by year")

# Frequences de plantes 
#- Types d'arbres  
U <- dataPiedsArbres_2014_2018%>% group_by(TB_taxa) %>%summarise(PlantFreq = n(),.groups = 'drop') 
U <- U %>% filter(PlantFreq > quantile(PlantFreq,0.9))  %>% filter(TB_taxa!= '(na)')  %>% mutate(TB_taxa = factor(TB_taxa, levels = TB_taxa[order(PlantFreq,decreasing  = FALSE)]))
ggplot(U, aes(x=TB_taxa,weight=PlantFreq))+geom_bar() +coord_flip()+ ggtitle("10% More Frequent Plant Species")

### descripteurs  Sites
# rue / grill ou pas / quel type d'arbres
descriptSites_2014_2018 <- dataPiedsArbres_2014_2018 %>%group_by(site) %>% filter(row_number() == 1) %>% select(site,TB_soil.grill,STREET,soil,TB_tree) %>% arrange(site)

###############" CREATE one matrix plant - site
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

plotMyMatrix(matSitePlant_2014_2018,dimLabels = c('Sites','Plants'))
dim(matSitePlant_2014_2018) 


##############################################################"
#------------- Binary Bipartite Bern  
#################################################################
#------------------ data
matSitePlant_bin_2014_2018 <- 1*(matSitePlant_2014_2018>0)
plotMyMatrix(matSitePlant_bin_2014_2018,dimLabels =  c('Sites','Plants'))

hist(rowSums(matSitePlant_bin_2014_2018))
hist(colSums(matSitePlant_bin_2014_2018))

#--------------  Estimation
#mySBM_bern_2014_2018 <- estimateBipartiteSBM(matSitePlant_bin_2014_2018,dimLabels = c('Sites','Plants'))
#save(mySBM_bern_2014_2018,file='res/resSBM_Bern_2014_2018.Rdata')
load('res/resSBM_Bern_2014_2018.Rdata')
plot(mySBM_bern_2014_2018,plotOptions = list(line.width = 1/10))

#---------------- Plot
## plot connexion matrix
longData_2014_2018<-melt(mySBM_bern_2014_2018$connectParam)
longData_2014_2018<-longData_2014_2018[longData_2014_2018$value!=0,]
ggplot(longData_2014_2018, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) +   scale_fill_gradient(low="grey90", high="red") +   labs(x="Plant Blocks", y="Site blocks", title="Connexion matrix") +
  theme_bw() +coord_equal()+ theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

#------------- Exploitation groupes 


##Ajouts de moi pour exporter la composition de chaque groupe
df1 <- data.frame(Nom_pa = names(SitePlant_2014_2018))
df1 <- cbind(df1,mySBM_bern_2014_2018$indMemberships$row)
colnames(df1) <- c('Nom_pa',paste0('gr_',1:4))
print(df1)

write.csv(df1,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/groupes_sites_2014_2018.csv", row.names = TRUE)

df2 <- data.frame(espece = colnames(matSitePlant_2014_2018))
df2 <- cbind(df2,mySBM_bern_2014_2018$indMemberships$col)
colnames(df2) <- c('espece',paste0('gr_',1:13))
print(df2)
#write.csv(df2,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/groupes_plantes_2014_2018.csv", row.names = TRUE)

#Meme chose mais en incluant les probas d'appartenance a chacun des groupes
df1 <- data.frame(Nom_pa = names(SitePlant_2014_2018))
df1 <- cbind(df1,mySBM_bern_2014_2018$probMemberships$row)
colnames(df1) <- c('Nom_pa',paste0('gr_',1:4))
print(df1)
#write.csv(df1,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/res_probas_appartenance_(26_09_2022)/groupes_sites_2014_2018.csv", row.names = TRUE)

df2 <- data.frame(espece = colnames(matSitePlant_2014_2018))
df2 <- cbind(df2,mySBM_bern_2014_2018$probMemberships$col)
colnames(df2) <- c('espece',paste0('gr_',1:13))
print(df2)
write.csv(df2,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/res_probas_appartenance_(26_09_2022)/groupes_plantes_2014_2018.csv", row.names = TRUE)



##############################################################"
#------------- Poisson Bipartite Bern  
#################################################################
#------------------ data
plotMyMatrix(matSitePlant_2014_2018,dimLabels =  c('Sites','Plants'))

hist(rowSums(matSitePlant_bin_2014_2018))
hist(colSums(matSitePlant_bin_2014_2018))

#--------------  Estimation
#mySBM_pois_2014_2018 <- estimateBipartiteSBM(matSitePlant_2014_2018,model = 'poisson',dimLabels = c('Sites','Plants'))
#save(mySBM_pois_2014_2018,file='res/resSBM_Pois_2014_2018.Rdata')
load('res/res_bipartitesbm_2014_2018.Rdata')
plot(mySBM_pois_2014_2018,plotOptions = list(line.width = 1/10))

#---------------- Plot
## plot connexion matrix
longData_2014_2018<-melt(mySBM_pois_2014_2018$connectParam)
longData_2014_2018<-longData_2014_2018[longData_2014_2018$value!=0,]
ggplot(longData_2014_2018, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) +   scale_fill_gradient(low="grey90", high="red") +   labs(x="Plant Blocks", y="Site blocks", title="Connexion matrix") +
  theme_bw() +coord_equal()+ theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                                   axis.text.y=element_text(size=9),
                                   plot.title=element_text(size=11))

###############################################################
#------------ Compar clustering of Poisson and Bern
###############################################################
mySBM_bern_2014_2018$memberships$Sites
plotAlluvial(list(Bern=mySBM_bern_2014_2018$memberships$Sites,Pois=mySBM_pois_2014_2018$memberships$Sites))
plotAlluvial(list(Bern=mySBM_bern_2014_2018$memberships$Plants,Pois=mySBM_pois_2014_2018$memberships$Plants))


##################################################################
#------------------ Utilisation des covariables sur les sites?
###################################################################

#------------- Grid or Soil
plotAlluvial(list(ClustBin=mySBM_bern_2014_2018$memberships$Sites,descriptSites_2014_2018$TB_soil.grill))
plotAlluvial(list(ClustPois=mySBM_pois_2014_2018$memberships$Sites,descriptSites_2014_2018$TB_soil.grill))

#------------- Street
plotAlluvial(list(ClustPois=mySBM_bern_2014_2018$memberships$Sites,descriptSites_2014_2018$STREET))
table(ClustBin=mySBM_bern_2014_2018$memberships$Sites,STREET=descriptSites_2014_2018$STREET)

#---------------Species
plotAlluvial(list(ClustBin=mySBM_bern_2014_2018$memberships$Sites,TreeSpecies=descriptSites_2014_2018$TB_tree,STREET=descriptSites_2014_2018$STREET))
table(ClustBin=mySBM_pois_2014_2018$memberships$Sites,STREET=descriptSites_2014_2018$STREET)

