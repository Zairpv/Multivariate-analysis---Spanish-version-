################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################

#1. CARGAR DATOS ---------------------------------------------------------------
rm(list=ls())

datos<-read.csv("data/4_datos paper.csv",row.names = 1)
colSums(is.na(datos))
View(datos)
names(datos)

#Librerias
library(psych)

#Selecci?n de variables y renombrar 
datos2<-datos[,25:47]
View(datos2)
names(datos2)<-c("Y","V1","V2","V3","V4","V5","V6","V7","V8","V9","V10",
                 "V11","V12","V13","V14","V15","V16","V17","V18","V19","V20",
                 "V21","V22")
names(datos2)
View(datos2)

#Limpieza de NA en los datos
datos_completos<-na.omit(datos2)
colSums(is.na(datos_completos))
View(datos_completos)

#Estandarizar los datos
escalados<-scale(datos_completos)
View(escalados)

#Matriz de correlaciones
correlac<-round(cor(escalados),2)
correlac
cov(escalados)
cor.plot(correlac, cex=0.5)

#Prueba de Bartlett --- Ho: la matriz de correlaciones es una matriz identidad
cortest.bartlett(correlac)  #como pvalue<0.05, se rechaza Ho

#Medida de adecuaci?n de la muestra KMO
KMO(escalados)

#Extracci?n de factores
scree(escalados)
factores1<-principal(escalados,nfactors=6,rotate = "none")
print(factores1,digits = 2)

factores2<-factanal(escalados,nfactors=3,rotation =  = "promax")
print(factores2,digits = 2)

#Comunalidades
factores2$communality

#Puntuaciones de factores
scores<-factores2$scores[,1:3]
head(scores,10)
plot(scores)
scores2<-factor.scores(escalados,factores2)
head(scores2$scores,10)

#Gr?fica biplot de variables y factores
plot(factores2,labels=names(escalados))
fa.diagram(factores2,labels = names(escalados))

#ajuste del modelo
factor.fit(correlac,factores2$loadings) #con tres factores
factor.fit(correlac,factores1$loadings) #con seis factores

#Extracci?n de las cargas
factores2$loadings