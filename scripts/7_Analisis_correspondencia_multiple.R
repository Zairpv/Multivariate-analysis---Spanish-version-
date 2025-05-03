################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################


# Librerias 
library(GDAtools)
library(ca)
library(FactoMineR)
library(ggplot2)
library(factoextra)

#Datos
datos_trigo<-read.csv("data/7_datos_trigo.csv",header = TRUE)
View(datos_trigo)
names(datos_trigo)
options(max.print = 10000)  

datos_trigo$IC<-as.factor(datos_trigo$IC)
datos_trigo$Phos<-as.factor(datos_trigo$Phos)
datos_trigo$Sid<-as.factor(datos_trigo$Sid)
trigo<-data.frame(datos_trigo$IC,datos_trigo$Phos,datos_trigo$Sid,datos_trigo$Genera)
names(trigo)<-c("IC","Phos","Sid","Genera")
View(trigo)

#Analisis de correspondencia multiple (Primer forma)
burt_m1<-mjca(trigo,lambda="Burt")
plot(burt_m1)
summary(burt_m1)

?MCA()

#Analisis de correspondencia multiple (Segunda forma)
MCA<-MCA(trigo,method="Burt")
MCA

#tabla de eigenvalores
MCA$eig 
MCA$var$coord
MCA$ind$coord
MCA$var$contrib
MCA$ind$contrib

plot.MCA(MCA)
plotellipses(MCA,keepvar = "all",cex=0.8,pch=20,autoLab = "yes",graph.type = "ggplot")
plotellipses(MCA,keepvar = "Genera",cex=0.8,pch=20)
plotellipses(MCA,keepvar = "IC",cex=0.8,pch=2)
plotellipses(MCA,keepvar = "Phos",cex=0.8,pch=2)
plotellipses(MCA,keepvar = "Sid",cex=0.8,pch=2)
