#statistiche per anlaizzare i clusters
statcluster <- read.table("D:/Users/epolidoro/Desktop/CzRM/MATTEO/cluster_finale_anagr_pages_20160829.csv", header=T, sep=",")
#per eliminare un file nella finestra DATA scrivere nella CONSOLE sotto: rm(novefile),
#se voglio eliminare tutti i file: rm(list=ls())
#per fare le statistiche divido secondo i clusters
library(dplyr)
cluster_1 <- statcluster %>% filter(risultato==1)
cluster_2 <- statcluster %>% filter(risultato==2)
cluster_3 <- statcluster %>% filter(risultato==3)
cluster_4 <- statcluster %>% filter(risultato==4)
cluster_5 <- statcluster %>% filter(risultato==5)
cluster_6 <- statcluster %>% filter(risultato==6)
cluster_7 <- statcluster %>% filter(risultato==7)
cluster_8 <- statcluster %>% filter(risultato==8)
cluster_9 <- statcluster %>% filter(risultato==9)
#statistiche perogni cluster
summary(cluster_1)
library(ggplot2)
plt <- ggplot(cluster_1, aes(CITT_LIVELLOSTUDIO_GRP_DES))
plt+geom_bar()
## df_1 (per fare statistiche sulle pagine visitate dal cluster 1)
cluster_1_pages <- cluster_1[,1:93]
pagename <- list()
totaltime <- list()
for(i in 2:ncol(cluster_1_pages)){
  totaltime <- rbind(totaltime, colSums(cluster_1_pages[i]))
  pagename <- rbind(pagename, colnames(cluster_1_pages[i]))
}
df_1 <- as.data.frame(cbind(pagename,totaltime))
####togliamo i valori =0
df_1 <- df_1 %>% filter(approfondimenti_attestatilinguis!=0)
df_1 <- df_1 %>% select(V1,a=approfondimenti_attestatilinguis)
df_1$V1 <- as.character(df_1$V1)
df_1$a <- as.numeric(df_1$a)
plt_pages1 <- ggplot(df_1, aes(x=V1,y=a))
plt_pages1+geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#df <- df %>% filter(V1 != "aziende_pagine")

## df_2 (per fare statistiche sulle pagine visitate dal cluster 2)
cluster_2_pages <- cluster_2[,1:93]
pagename <- list()
totaltime <- list()
for(i in 2:ncol(cluster_1_pages)){
  totaltime <- rbind(totaltime, colSums(cluster_2_pages[i]))
  pagename <- rbind(pagename, colnames(cluster_2_pages[i]))
}
df_2 <- as.data.frame(cbind(pagename,totaltime))
###togliamo i valori =0
df_2 <- df_2 %>% filter(approfondimenti_attestatilinguis!=0)
df_2 <- df_2 %>% select(V1,a=approfondimenti_attestatilinguis)
df_2$V1 <- as.character(df_2$V1)
df_2$a <- as.numeric(df_2$a)
plt_pages2 <- ggplot(df_2, aes(x=V1,y=a))
plt_pages2+geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#df <- df %>% filter(V1 != "aziende_pagine")
