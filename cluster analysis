#Analisi Cluster user e pages
#install.packages("flexclust")
cluster <- read.table("D:/Users/epolidoro/Desktop/CzRM/clus_user_pages new_23082016.csv", header=T, sep=",")
str(cluster)
#per vedere etichetta varibiali
cluster[1:4,132:135]
#prendo tutte le colonne tranne quelle selezionate (e carico il pacchetto dplyr per fare %<%)
library(dplyr)
cluster_2 <- cluster %>% select(-X_clus_1,-X_clus_2,-X_clus_3,-X_clus_4,-X_clus_5,-X_clus_6,-X_clus_7,-X_clus_8,-X_clus_9,-X_clus_10,-X_clus_11,-X_clus_12)
#cluster analysis
library(flexclust)
set.seed(5)
numero_cluster <- 5
mediana <- kcca(cluster_2, numero_cluster, family=kccaFamily("kmedians"), weights=NULL, group=NULL,
                control=NULL, simple=FALSE, save.data=FALSE)
cluster5 <- clusters(mediana)
#cambio la struttura della variabile: da numerico a fattore
cluster5 <- as.factor(cluster5)
summary(cluster5)
#proviamo con "angle" 5 cluster
medianaangle <- kcca(cluster_2, numero_cluster, family=kccaFamily("angle"), weights=NULL, group=NULL,
                control=NULL, simple=FALSE, save.data=FALSE)
cluster5a <- clusters(medianaangle)
cluster5a <- as.factor(cluster5a)
summary(cluster5a)
#proviamo con "angle" 8 cluster
numero_cluster <- 8
medianaangle8 <- kcca(cluster_2, numero_cluster, family=kccaFamily("angle"), weights=NULL, group=NULL,
                     control=NULL, simple=FALSE, save.data=FALSE)
cluster8a <- clusters(medianaangle8)
cluster8a <- as.factor(cluster8a)
summary(cluster8a)
#proviamo con "angle" 12 cluster
numero_cluster <- 12
medianaangle12 <- kcca(cluster_2, numero_cluster, family=kccaFamily("angle"), weights=NULL, group=NULL,
                      control=NULL, simple=FALSE, save.data=FALSE)
cluster12a <- clusters(medianaangle12)
cluster12a <- as.factor(cluster12a)
summary(cluster12a)
#proviamo con "angle" 4 cluster
numero_cluster <- 4
medianaangle4 <- kcca(cluster_2, numero_cluster, family=kccaFamily("angle"), weights=NULL, group=NULL,
                      control=NULL, simple=FALSE, save.data=FALSE)
cluster4a <- clusters(medianaangle4)
cluster4a <- as.factor(cluster4a)
summary(cluster4a)
#proviamo con "angle" 9 cluster
numero_cluster <- 9
medianaangle9 <- kcca(cluster_2, numero_cluster, family=kccaFamily("angle"), weights=NULL, group=NULL,
                      control=NULL, simple=FALSE, save.data=FALSE)
cluster9a <- clusters(medianaangle9)
cluster9a <- as.factor(cluster9a)
summary(cluster9a)
#proviamo con "kmedians" 9 cluster
numero_cluster <- 9
mediana <- kcca(cluster_2, numero_cluster, family=kccaFamily("kmedians"), weights=NULL, group=NULL,
                control=NULL, simple=FALSE, save.data=FALSE)
cluster9 <- clusters(mediana)
cluster9 <- as.factor(cluster9)
summary(cluster9)
#gplot cluster
library(ggplot2)
plt <- ggplot(cluster_2, aes(x=cluster8a))
plt+geom_bar()
plt <- ggplot(cluster_2, aes(x=cluster12a))
plt+geom_bar()
plt <- ggplot(cluster_2, aes(x=cluster5a))
plt+geom_bar()
plt <- ggplot(cluster_2, aes(x=cluster9a))
plt+geom_bar()
plt <- ggplot(cluster_2, aes(x=cluster9))
plt+geom_bar()
#eliminare variabili anagrafiche
cluster_3 <- cluster_2[,1:93]
numero_cluster <- 9
mediana <- kcca(cluster_3, numero_cluster, family=kccaFamily("angle"), weights=NULL, group=NULL,
                control=NULL, simple=FALSE, save.data=FALSE)
cluster9p <- clusters(mediana)
cluster9p <- as.factor(cluster9p)
summary(cluster9p)
cluster_x <- 
cluster_finale <- cbind(cluster_3,cluster_2[,94:123])
cluster_with_anagrafiche$risultato <- cluster9p
library(xlsx)

anagraf <- read.table("D:/Users/epolidoro/Desktop/CzRM/Utenti_Pagine_Anagrafice_CIT_2208.csv", header=T, sep=",")
anagraf2 <- anagraf[,95:100]
cluster_with_anagrafiche <- cbind(cluster_3,anagraf2)
write.csv(cluster_with_anagrafiche, "cluster_finale_anagrafiche.csv",
row.names = F)
write.xlsx(cluster_finale, "cluster_finale.xlsx", sheetName = "Folglio1",
           row.names = F)
