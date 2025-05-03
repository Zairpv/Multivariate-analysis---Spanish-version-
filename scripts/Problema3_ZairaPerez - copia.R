################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################



#Conjunto de datos
rm(list=ls())
datos<-read.csv("data/datos problema3.csv",header=TRUE)
View(datos)
options(max.print = 10000)

#Librerias
library(psych)
library(factoextra)
library(FactoMineR)
library(GPArotation)
library(psy)
library(dplyr)

names(datos)
datos_var<-datos[,8:13] #Hull transformacion, los datos proporcionados ya estan escalados

## 1. Comprobaci?n de supuestos
#Prueba de normalidad
shapiro.test(datos_var$Var1) #Es normal
shapiro.test(datos_var$Var2) #Es normal
shapiro.test(datos_var$Var3) #Es normal
shapiro.test(datos_var$Var4) #Es normal
shapiro.test(datos_var$Var5) #Es normal
shapiro.test(datos_var$Var6) #No es normal

#Matriz de correlaciones
correlaciones<-cor(datos_var)
round(correlaciones,3)
covarianzas<-cov(datos[,2:7])

#Medida de adecuaci?n KMO
KMO(datos_var)

#Prueba de bartlet
cortest.bartlett(correlaciones,length(datos$Var1),diag = FALSE)

#Grafica de sedimentacion
scree(datos_var,factors=TRUE)

#2. Extracci?n de componentes por PCA
#Nota: Las variables utilizadas ya estaban previamente escaladas por los autores
?prcomp
Compon<-prcomp(datos_var,scale. = FALSE)
summary(Compon)
Compon
Compon$sdev
Compon$x
biplot(Compon,scale=0)

?PCA
comp2<-PCA(datos_var,scale=FALSE,ncp=6)
comp2$eig
fviz_screeplot(comp2,addlabels=FALSE,geom="line")
plot(comp2,choix = "ind",cex=0.7)
plot.PCA(comp2,axes=c(1,2),choix = "var",habillage = 2,cex=0.7,ellipse = TRUE)
paleta<-c("#451460","#56b893","#a9c12a")
fviz_pca_var(comp2,col.var = "cos2",gradient.cols=paleta,repel=TRUE)


#3. Extracci?n de factores sin rotaci?n

facts1<-principal(datos_var,nfactors = 6,rotate = "none")
summary(facts1)
print(facts1,digits = 3) 
facts1$loadings
facts1$communality
facts1$values
facts1$scores

f1<-facts1$scores[,1]
f2<-factores1$scores[,2]
plot(f1,f2)

fa.diagram(facts1,labels = names(datos_var))
plot(facts1$values, type = "b",xlab = "Factors", ylab = "Eigen values", main = "SCREE PLOT") 

#Ajuste del modelo
factor.fit(correlaciones,facts1$loadings[,1:2]) #considerando 2 componentes

# 4. Rotaci?n de factores

#Rotaci?n ortogonal
facts_Rotados<-principal(datos_var,nfactors = 2,rotate = "varimax")
summary(facts_Rotados)
print(facts_Rotados,digits = 3) 
facts_Rotados$loadings
facts_Rotados$communality
facts_Rotados$values
facts_Rotados$scores
fa.diagram(facts_Rotados,labels = names(datos_var))

#Rotaci?n oblicua
facts_Rotados2<-principal(datos_var,nfactors = 2,rotate = "oblimin")
print(facts_Rotados,digits = 3) 
facts_Rotados2$loadings
facts_Rotados2$communality
facts_Rotados2$values
facts_Rotados2$scores
fa.diagram(facts_Rotados2,labels = names(datos_var))

#Comparaci?n de comunalidades
facts1$communality
facts_Rotados$communality
facts_Rotados2$communality

#5. Obtenci?n de factores (AI Structure)
#Sustituir en la ecuaci?n
#0.957*Var1+0.955*Var4+0.806*Var3-0.949*Var5-0.948*Var2       
Factor1<-(0.957*datos_var$Var1)+(0.955*datos_var$Var4)+(0.806*datos_var$Var3)-(0.949*datos_var$Var5)-(0.948*datos_var$Var2)      
Factor2<-0.967*(datos_var$Var6)
AI_structure<-Factor1+Factor2

AI_Structure_Rumania<-data.frame(datos$Counties,Factor1,Factor2,AI_structure)          
View(AI_Structure_Rumania)
?arrange
AI_Structure_Rumania_Ordenada<-arrange(AI_Structure_Rumania,desc(AI_structure))
AI_Structure_Rumania_Ordenada

#6. Kmeans
set.seed(101010)

?hkmeans()
paleta<-c("#046e6c","#fbb29e","#80c1c5","#800080","#926343","#3b7d5c")
res.hk<-hkmeans(AI_Structure_Rumania_Ordenada[,2:4],6)
res.hk
res.hk$cluster
res.hk$size
fviz_cluster(res.hk,ggtheme = theme_minimal(),palette=paleta)

paleta_ordenada<-c("#fbb29e","#046e6c","#3b7d5c","#926343","#80c1c5","#800080")
# dendrograma
fviz_dend(res.hk, cex = 0.6,  rect = TRUE, rect_fill = TRUE,palette = paleta_ordenada,rect_border = paleta_ordenada)

#Agrupamiento jerarquico
d=dist(AI_Structure_Rumania_Ordenada,method = "euclidian")
clus1<-hclust(d,method = "ward.D2")
subgrups <- cutree(clus1, k = 6)
clus1
subgrups
table(AI_Structure_Rumania_Ordenada$datos.Counties,as.factor(subgrups))

#7. Numero optimo de clusteres
fviz_nbclust(AI_Structure_Rumania_Ordenada[,2:4], FUN = kmeans, method = "wss")
fviz_nbclust(AI_Structure_Rumania_Ordenada[,2:4], FUN = kmeans, method = "silhouette")
