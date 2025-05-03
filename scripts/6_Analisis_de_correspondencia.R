################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################


#Library
library(pander)
library(ca)
library(FactoMineR)
library(factoextra)
library(amap)

#Creaci?n de la tabla de contingencia
#Tabla IV
database<-matrix(c(12,6,0,3,6,9,12,4,2,12,3,3,0,
                   9,9,3,2,7,12,9,6,6,7,0,11,3,
                   3,10,8,7,11,3,3,8,10,1,11,3,6,
                   0,2,16,15,3,0,0,9,9,0,0,9,9),ncol = 4)
database
colnames(database)<-c("F1","F2","F3","F4")
rownames(database)<-c("I1","I2","I3",
                      "L1","L2","L3",
                      "S1","S2","S3",
                      "R1","R2","R3","R4")
database

ILS_data<-database[1:9,]
I_data<-database[1:3,]
L_data<-database[4:6,]
S_data<-database[7:9]
R_data<-database[10:13,]

###Prueba X2
#Para ILS
chisq.test(ILS_data)
#Para I
chisq.test(I_data)
#Para L
chisq.test(L_data)

###An?lisis de correspondencia
ca_ils<-ca(ILS_data)
ca_ils
#raiz cuadrada de eigenvalores
ca_ils$sv
#Coordenadas de renglones
ca_ils$rowcoord

#Coordenadas de columnas
ca_ils$colcoord

############
ca_ils$colinertia

#Gr?fica
plot(ca_ils)

#### Con la libreria FactoMine
ca2_ils<-CA(ILS_data)
ca2_ils

ca2_ils$eig  #inercia considerada por los ejes (% de varianza)
ca2_ils$row$coord #Coordenadas
ca2_ils$col$coord
ca2_ils$col$contrib
ca2_ils$row$contrib
(ca2_ils$col$cos2)*100
(ca2_ils$row$cos2)*100

fviz_ca_biplot(ca2_ils)

