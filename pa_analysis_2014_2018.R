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
dataPiedsArbres_2014_2018 <- dataPiedsArbres_2014_2018 %>% mutate(TB_taxa = fct_collapse(A = c("A","B")))

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

### descript Sites
# rue / grill ou pas

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

#------------- Binary 
matSitePlant_bin_2014_2018 <- 1*(matSitePlant_2014_2018>0)
plotMyMatrix(matSitePlant_bin_2014_2018,dimLabels =  c('Sites','Plants'))

hist(rowSums(matSitePlant_bin_2014_2018))
hist(colSums(matSitePlant_bin_2014_2018))

############################################ 
mySBM_bern_2014_2018 <- estimateBipartiteSBM(matSitePlant_bin_2014_2018,dimLabels = c('Sites','Plants'))
save(mySBM_bern_2014_2018,file='res/resSBM_Bern_2014_2018.Rdata')
plot(mySBM_bern_2014_2018,type = c("expected"),plotOptions = list(line.width = 1/10))


## plot conneciton matrix
longData_2014_2018<-melt(mySBM_bern_2014_2018$connectParam)
longData_2014_2018<-longData_2014_2018[longData_2014_2018$value!=0,]
ggplot(longData_2014_2018, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="letters", y="LETTERS", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


mySBM_2014_2018 <-defineSBM(matSitePlant_bin_2014_2018,
                            model = "bernoulli",
                            dimLabels = c('Plants','Sites'))

mySBM_bern_2_2014_2018 <- estimateMultipartiteSBM_2014_2018(list(mySBM))


mySBM_bern_2014_2018$blockProp$col*100

##Ajouts de moi pour exporter la composition de chaque groupe
df1 <- data.frame(Nom_pa = names(SitePlant_2014_2018), gr_1 = mySBM_bern_2014_2018$indMemberships$row[,1],
                  gr_2 = mySBM_bern_2014_2018$indMemberships$row[,2],
                  gr_3 = mySBM_bern_2014_2018$indMemberships$row[,3], 
                  gr_4 = mySBM_bern_2014_2018$indMemberships$row[,4])
print(df1)

write.csv(df1,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/groupes_sites_2014_2018.csv", row.names = TRUE)

df2 <- data.frame(espece = colnames(matSitePlant_2014_2018), gr_1 = mySBM_bern_2014_2018$indMemberships$col[,1],
                  gr_2 = mySBM_bern_2014_2018$indMemberships$col[,2],
                  gr_3 = mySBM_bern_2014_2018$indMemberships$col[,3],
                  gr_4 = mySBM_bern_2014_2018$indMemberships$col[,4],
                  gr_5 = mySBM_bern_2014_2018$indMemberships$col[,5],
                  gr_6 = mySBM_bern_2014_2018$indMemberships$col[,6],
                  gr_7 = mySBM_bern_2014_2018$indMemberships$col[,7],
                  gr_8 = mySBM_bern_2014_2018$indMemberships$col[,8],
                  gr_9 = mySBM_bern_2014_2018$indMemberships$col[,9],
                  gr_10 = mySBM_bern_2014_2018$indMemberships$col[,10],
                  gr_11 = mySBM_bern_2014_2018$indMemberships$col[,11],
                  gr_12 = mySBM_bern_2014_2018$indMemberships$col[,12],
                  gr_13 = mySBM_bern_2014_2018$indMemberships$col[,13])

print(df2)

write.csv(df2,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/groupes_plantes_2014_2018.csv", row.names = TRUE)

#Meme chose mais en incluant les probas d'appartenance a chacun des groupes
df1 <- data.frame(Nom_pa = names(SitePlant_2014_2018), gr_1 = mySBM_bern_2014_2018$probMemberships$row[,1],
                  gr_2 = mySBM_bern_2014_2018$probMemberships$row[,2],
                  gr_3 = mySBM_bern_2014_2018$probMemberships$row[,3], 
                  gr_4 = mySBM_bern_2014_2018$probMemberships$row[,4])
print(df1)

write.csv(df1,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/res_probas_appartenance_(26_09_2022)/groupes_sites_2014_2018.csv", row.names = TRUE)

df2 <- data.frame(espece = colnames(matSitePlant_2014_2018), gr_1 = mySBM_bern_2014_2018$probMemberships$col[,1],
                  gr_2 = mySBM_bern_2014_2018$probMemberships$col[,2],
                  gr_3 = mySBM_bern_2014_2018$probMemberships$col[,3],
                  gr_4 = mySBM_bern_2014_2018$probMemberships$col[,4],
                  gr_5 = mySBM_bern_2014_2018$probMemberships$col[,5],
                  gr_6 = mySBM_bern_2014_2018$probMemberships$col[,6],
                  gr_7 = mySBM_bern_2014_2018$probMemberships$col[,7],
                  gr_8 = mySBM_bern_2014_2018$probMemberships$col[,8],
                  gr_9 = mySBM_bern_2014_2018$probMemberships$col[,9],
                  gr_10 = mySBM_bern_2014_2018$probMemberships$col[,10],
                  gr_11 = mySBM_bern_2014_2018$probMemberships$col[,11],
                  gr_12 = mySBM_bern_2014_2018$probMemberships$col[,12],
                  gr_13 = mySBM_bern_2014_2018$probMemberships$col[,13])

print(df2)

write.csv(df2,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/res_probas_appartenance_(26_09_2022)/groupes_plantes_2014_2018.csv", row.names = TRUE)
