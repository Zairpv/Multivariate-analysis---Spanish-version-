################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################

# 1. LIBRERIAS -----------------------------------------------------------------
#install.packages("psych","corrplot","PerformanceAnalytics","devtools","ggbiplot","","","","")
library(psych)
library(corrplot)
library(PerformanceAnalytics)
library(devtools)
library(ggbiplot)
library(factoextra)
library(FactoMineR)
library(mxmaps)
library(ggplot2)

# 2. CARGAR DATOS ----------------------------------------------------------------- 
options(max.print = 10000)
datos_inegi<-read.csv("data/3_datos_basura.csv",header=TRUE,row.names = 1)
View(datos_inegi)
names(datos_inegi)
names(datos_inegi)<-c("TM","V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11",
                      "V12","V13","V14","V15","V16","V17","V18","V19","V20")
names(datos_inegi)

#a) Revisar las variables incluidas en el estudio
summary(datos_inegi[2:21])
describeBy(datos_inegi[2:21], digits = 4)

#b) Analisis de correlaciones
matriz_cor<-round(cor(datos_inegi[2:21],method = "pearson"),2)
matriz_cor
corrplot(matriz_cor)
chart.Correlation(datos_inegi[2:21],histogram = TRUE,pch=10,cex=0.6)
heatmap(x=cor(datos_inegi[2:21]),symm = TRUE)

#Analisis de componentes principales
rownames(datos_inegi)
comp_prin_rsu<-prcomp(datos_inegi[2:21],center = TRUE,scale. = TRUE)
summary(comp_prin_rsu)
comp_prin_rsu

?PCA()
comp_prin_rsu2<-PCA(datos_inegi[2:21],scale.unit = TRUE,graph = TRUE)
get_eig(comp_prin_rsu2) #Comparable con la tabla del art?culo

#Evaluacion de componentes en datos
componentes2<-predict(comp_prin_rsu2,newdata = datos_inegi)
componentes2
biplot(comp_prin_rsu,cex=0.5)
eigval<-get_eigenvalue(comp_prin_rsu)
eigval

#Grafica de eigenvalores
plot(comp_prin_rsu,type="l",main="Componentes principales RSU")
fviz_screeplot(comp_prin_rsu2,addlabels=FALSE,ylim=c(0,50),ncp=20,geom="line",
               xlab="Dimensiones",ylab="Porcentaje de varianza explicada")
plot(comp_prin_rsu2,choix="ind",cex=0.7)
plot.PCA(comp_prin_rsu2, axes=c(1, 2), choix="var", habillage=2,cex=0.7,ellipse = TRUE)
paleta=c("#451460","#56b893","#a9c12a")
fviz_pca_var(comp_prin_rsu2,col.var = "cos2",gradient.cols=paleta,repel = TRUE)

#Extraer los resultados de las variables
var_rsu<-get_pca_var(comp_prin_rsu2)
var_rsu
var_rsu$coord
var_rsu$contrib
fviz_contrib(comp_prin_rsu2, choice = "var", axes = 1, top = 10)

#Mapa de M?xico
#Evaluacion de componentes en datos

names(df_mxstate)
estados<-df_mxstate$state_name
componentes1<-predict(comp_prin_rsu,newdata = datos_inegi)


df_mxstate$value <- componentes1[,1] #selecci?n del CP1
df_mxstate$value

mxstate_choropleth(df_mxstate,
                   title = "Residuos solidos urbanos",legend = "PC1")
?mxstate_choropleth
