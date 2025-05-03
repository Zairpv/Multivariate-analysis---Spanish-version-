################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################


#Conjunto de datos
datos<-read.csv("data/9_Datos_T2.csv")
View(datos)
names(datos)

#Library
library(ICSNP)
library(rrcov)

datos$M?todo<-as.factor(datos$M?todo)
levels(datos$M?todo)
Mvisual<-datos[1:9,3:5]
Mvisual<-as.matrix(Mvisual)
Macust<-datos[10:18,3:5]
Macust<-as.matrix(Macust)
Macust
Mvisual
#Diferencias
Mat_Dif<-Macust-Mvisual
Mat_Dif


#T2 Hotelling
media0<-c(0,0,0)
HotellingsT2(Mat_Dif,mu=media0) 
T2.test(Mat_Dif,mu=media0)

