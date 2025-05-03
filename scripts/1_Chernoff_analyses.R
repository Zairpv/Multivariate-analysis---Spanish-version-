################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################


#1. PAQUETES  ------------------------------------------------------------------
library(aplpack)
library(readr)

# 2. CONECTAR FOLDER ------------------------------------------------------------
datos<-read.csv("data/1_public_utility_data.csv",header = TRUE)
View(datos)
summary(datos)

# 3. ANALISIS ------------------------------------------------------------------
##Caras de Chernoff sin clusters
cher<-faces(datos[5:12],main = "Chernoff faces",labels=datos$label,cex = 0.9,face.type=0)

##Caras de Chernoff en clusters
cluster1<-datos[datos$cluster=="C1",]
cluster2<-datos[datos$cluster=="C2",]
cluster3<-datos[datos$cluster=="C3",]
cluster4<-datos[datos$cluster=="C4",]
cluster5<-datos[datos$cluster=="C5",]
cluster6<-datos[datos$cluster=="C6",]
cluster7<-datos[datos$cluster=="C7",]

cher_c1<-faces(cluster1[5:12],main = "Cluster 1",labels=cluster1$label,cex = 0.9,ncol.plot = 1,nrow.plot = 4,face.type = 0)
cher_c2<-faces(cluster2[5:12],main = "Cluster 2",labels=cluster2$label,cex = 0.9,ncol.plot = 1,nrow.plot = 6,face.type = 0)
cher_c3<-faces(cluster3[5:12],main = "Cluster 3",labels=cluster3$label,cex = 0.9,ncol.plot = 1,nrow.plot = 2,face.type = 0)
cher_c4<-faces(cluster4[5:12],main = "Cluster 4",labels=cluster4$label,cex = 0.9,ncol.plot = 1,nrow.plot = 3,face.type = 0)
cher_c5<-faces(cluster5[5:12],main = "Cluster 5",labels=cluster5$label,cex = 0.9,ncol.plot = 1,nrow.plot = 2,face.type = 0)
cher_c6<-faces(cluster6[5:12],main = "Cluster 6",labels=cluster6$label,cex = 0.9,ncol.plot = 1,nrow.plot = 3,face.type = 0)
cher_c7<-faces(cluster7[5:12],main = "Cluster 7",labels=cluster7$label,cex = 0.9,ncol.plot = 1,nrow.plot = 2,face.type = 0)


#Flu data
flu_datos<-read.csv("data/1_flu_data.csv",header = TRUE)
flu_datos

flu1<-flu_datos[1:7,]
flu1

#Chernoff faces sin considerar datos atipicos
flu_cher1<-faces(flu1[3:4],main = "Chernoff faces without atipics",cex=0.9)

flu2<-flu_datos[2:8,]
flu2

#Chernoff faces considerando datos atipicos
flu_cher2<-faces(flu2[3:4],main = "Chernoff faces with atipics",cex=0.9)

