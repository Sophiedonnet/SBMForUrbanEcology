rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(sbm)
library(reshape2)
source('functions_PiedsArbres.R')


#"------------------- données envoyées par ? 
dataPiedsArbres <- read.delim("TB 2009-2018.txt")

#-------------------- 
dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_taxa = as.factor(TB_taxa)) %>% mutate(soil = as.factor(TB_soil.grill))
dataPiedsArbres <- dataPiedsArbres %>% mutate(STREET_number  =  TB_number..in.the.street.)
dataPiedsArbres <- dataPiedsArbres %>% mutate(site = as.factor(paste(STREET,STREET_number,sep="_")))
dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_tree = as.factor(TB_tree))
dataPiedsArbres <- dataPiedsArbres %>% mutate(DATE = as.POSIXct(DATE, format = "%d/%m/%Y")) %>% mutate(YEAR = format(DATE, format="%Y")) %>%  mutate(MONTH = format(DATE, format="%m"))
dataPiedsArbres <- dataPiedsArbres %>% mutate(TB_taxa = fct_collapse(A = c("A","B")))



################### QUICK EXPLORATION
#- Types d'arbres  
dataPiedsArbres%>% group_by(site,TB_tree) %>%summarise(n = n(),.groups = 'drop') %>% ggplot(aes(x=TB_tree))+geom_bar() +coord_flip() +  scale_fill_brewer(palette='Greens') + ggtitle("Tree Species")

# Relevés par années
dataPiedsArbres%>% group_by(site,YEAR) %>%summarise(n = 1*(n()>1),.groups = 'drop') %>% ggplot(aes(x=YEAR))+geom_bar() +coord_flip() +  scale_fill_brewer(palette='Greens') + ggtitle("Nb of sites sampled  at least on time by year")

# Frequences de plantes 
#- Types d'arbres  
U <- dataPiedsArbres%>% group_by(TB_taxa) %>%summarise(PlantFreq = n(),.groups = 'drop') 
U <- U %>% filter(PlantFreq > quantile(PlantFreq,0.9))  %>% filter(TB_taxa!= '(na)')  %>% mutate(TB_taxa = factor(TB_taxa, levels = TB_taxa[order(PlantFreq,decreasing  = FALSE)]))
ggplot(U, aes(x=TB_taxa,weight=PlantFreq))+geom_bar() +coord_flip()+ ggtitle("10% More Frequent Plant Species")

### descript Sites
# rue / grill ou pas

###############" CREATE one matrix plant - site
SitePlant  <- dataPiedsArbres %>%  count(site,TB_taxa) %>% pivot_wider(names_from = site, values_from = n, values_fill = list(n = 0))
plantNames <- SitePlant$TB_taxa
SitePlant  <- SitePlant[,-1]
matSitePlant <- as.matrix(SitePlant)
colnames(matSitePlant) <- names(SitePlant)
rownames(matSitePlant) <- plantNames

w.rm <- c(which(plantNames=='(na)'),which(plantNames=='Sp.'),which(plantNames=='sp.?'))

matSitePlant <- matSitePlant[-w.rm,] # remove name plant = na
matSitePlant <- t(matSitePlant)
dim(matSitePlant)



plotMyMatrix(matSitePlant,dimLabels = c('Sites','Plants'))
dim(matSitePlant) 

#------------- Binary 
matSitePlant_bin <- 1*(matSitePlant>0)
plotMyMatrix(matSitePlant_bin,dimLabels =  c('Sites','Plants'))

hist(rowSums(matSitePlant_bin))
hist(colSums(matSitePlant_bin))

#-------------- Small 
#selec <- order(colSums(matSitePlant_bin),decreasing = TRUE)[1:100]
#smallMatSitePlant_bin <- matSitePlant_bin[,selec]
#plotMyMatrix(smallMatSitePlant_bin,dimLabels = c('site','Plants'))


############################################ 
mySBM_bern <- estimateBipartiteSBM(matSitePlant_bin,dimLabels = c('Sites','Plants'))
save(mySBM_bern,file='res/resSBM_Bern.Rdata')
plot(mySBM_bern,type = c("expected"),plotOptions = list(line.width = 1/10))

## plot conneciton matrix
longData<-melt(mySBM_bern$connectParam)
longData<-longData[longData$value!=0,]
ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="letters", y="LETTERS", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


mySBM <-defineSBM(matSitePlant_bin,
  model = "bernoulli",
  dimLabels = c('Plants','Sites'))

mySBM_bern_2 <- estimateMultipartiteSBM(list(mySBM))


mySBM_bern$blockProp$col*100

##Ajouts de moi pour exporter la composition de chaque groupe
df1 <- data.frame(Nom_pa = names(SitePlant), gr_1 = mySBM_bern$indMemberships$row[,1],
                  gr_2 = mySBM_bern$indMemberships$row[,2],
                  gr_3 = mySBM_bern$indMemberships$row[,3])
print(df1)

write.csv(df1,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/groupes_sites.csv", row.names = TRUE)

df2 <- data.frame(espece = colnames(matSitePlant), gr_1 = mySBM_bern$indMemberships$col[,1],
                  gr_2 = mySBM_bern$indMemberships$col[,2],
                  gr_3 = mySBM_bern$indMemberships$col[,3],
                  gr_4 = mySBM_bern$indMemberships$col[,4],
                  gr_5 = mySBM_bern$indMemberships$col[,5],
                  gr_6 = mySBM_bern$indMemberships$col[,6],
                  gr_7 = mySBM_bern$indMemberships$col[,7],
                  gr_8 = mySBM_bern$indMemberships$col[,8],
                  gr_9 = mySBM_bern$indMemberships$col[,9],
                  gr_10 = mySBM_bern$indMemberships$col[,10],
                  gr_11 = mySBM_bern$indMemberships$col[,11],
                  gr_12 = mySBM_bern$indMemberships$col[,12],
                  gr_13 = mySBM_bern$indMemberships$col[,13],
                  gr_14 = mySBM_bern$indMemberships$col[,14],
                  gr_15 = mySBM_bern$indMemberships$col[,15])

print(df2)

write.csv(df2,"/home/alouvet/Documents/PhD/project_communities_tree_bases/ECOLOGIE_URBAINE/res/groupes_plantes.csv", row.names = TRUE)
