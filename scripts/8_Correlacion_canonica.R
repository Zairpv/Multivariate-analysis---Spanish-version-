################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################

#Conjunto de datos

datos<-read.csv("data/8_datos.csv")
View(datos)
names(datos)
especies<-datos[,2:5]
productos<-datos[,6:9]

#Librerias
library(vegan)
library(CCA)
library(GGally)
library(PMA)

#Analisis descriptivo
summary(datos)
cor(especies)
cor(productos)
ggpairs(especies,title="Especies")
ggpairs(productos,title="Productos")
ggduo(datos,columnsX = 2:5,columnsY = 6:9,
      types = list(continuous = "smooth_lm"),
      title = "Correlaci?n entre variables Especies y  productos",
      xlab = "Cobertura de especies",
      ylab = "Concentraci?n de productos")
?ggduo()

#Correlaci?n canonina "paquete CCA"
cca_datos1<-matcor(especies,productos)
img.matcor(cca_datos1, type = 2)

cca_datos2<-cc(especies,productos)
cca_datos2$cor
cca_datos2$xcoef
plt.cc(cca_datos2,var.label=T,ind.names = datos[,1])

#Correlaci?n can?nica paquete vegan. 
cca_datos3<-CCorA(especies,productos)
cca_datos3
biplot(cca_datos3,xlabs=NA)
xlabs = dev.off()
