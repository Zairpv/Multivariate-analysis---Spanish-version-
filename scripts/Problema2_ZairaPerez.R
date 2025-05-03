#Examen 
#Problema 2
#Presenta Zaira Perez
rm(list())

#Lectura de datos
setwd("C:/Users/zaira/Documents/01 - Doctorado PCF/Otoño 2020/Analisis multivariado/Examen/Rscripts")
datos<-read.csv("datos problema2.csv")
View(datos)

#Librerias
library(ggplot2)
library(GGally)
library(CCA)
library(vegan)
library(CCP)
library(MVN)
library(PMA)
library(caret)
library(pls)
library(dplyr)
library(factoextra)

#1. Agrupamiento de variables
names(datos)<-c("Poblacion","PG","EH","EV","PER","G","GR","V","PN","PA","SM")
Xmat<-datos[,2:6]
View(Xmat)
Ymat<-datos[,7:11]
View(Ymat)

#2. Descriptivo
summary(Xmat)
summary(Ymat)

#3. Relación entre variables
png("P2_1.CorrelacionXvars.png",width = 700,height = 550)
ggpairs(Xmat)
dev.off()

png("P2_2.CorrelacionYvars.png",width = 700,height = 550)
ggpairs(Ymat)
dev.off()

#4. Correlacion canonica

Corr_can<-matcor(Xmat,Ymat)
Corr_can
img.matcor(Corr_can,type = 2)

Corr_can2<-cc(Xmat,Ymat)
Corr_can2
Corr_can2$cor #Correlaciones canonicas
Corr_can2[3:4] #Coeficientes canonicos
plt.cc(Corr_can2,var.label = T)

cargas_canonicas<-comput(Xmat,Ymat,Corr_can2)
cargas_canonicas[3:6]

?p.asym
p.asym((Corr_can2$cor),(dim(Xmat)[1]),(length(Xmat)),(length(Ymat)),tstat = "Wilks")
p.asym((Corr_can2$cor),(dim(Xmat)[1]),(length(Xmat)),(length(Ymat)),tstat = "Hotelling")
p.asym((Corr_can2$cor),(dim(Xmat)[1]),(length(Xmat)),(length(Ymat)),tstat = "Pillai")p.asym((Corr_can2$cor),(dim(Xmat)[1]),(length(Xmat)),(length(Ymat)),tstat = "Roy")

?cca #Libreria vegan
Corr_can3<-cca(Xmat,Ymat)
Corr_can3
plot(Corr_can3)

?CCorA
Corr_can4<-CCorA(Ymat,Xmat)
Corr_can4
Corr_can4$CanCorr
biplot(Corr_can4)

#5. PLS 

#Datos de entrenamiento y validacion
set.seed(12345)
indice<-createDataPartition(datos$Poblacion,p=0.75,list = FALSE,times = 1)
datos_entrenam<-datos[indice,]
datos_validac<-datos[-indice,]
View(datos_entrenam)
View(datos_validac)

Xmat_entrenamiento<-datos_entrenam[,2:6]
Xmat_entrenamiento<-as.matrix(Xmat_entrenamiento)
View(Xmat_entrenamiento)

Ymat_entrenamiento<-datos_entrenam[,7:11]
Ymat_entrenamiento<-as.matrix(Ymat_entrenamiento)
View(Ymat_entrenamiento)


#Modelo plsr
?plsr
plsFit = plsr(Ymat_entrenamiento~Xmat_entrenamiento, validation="CV",scale=TRUE,model=TRUE)
summary(plsFit)
class(plsFit)
plsFit$coefficients

plot(RMSEP(plsFit),main="Raiz del error para el modelo PLS")
plot(plsFit, plottype='coef',labels=names(datos_entrenam), ncomp=1:5)
plot(plsFit,plottype="correlation", labels=names(datos_entrenam),cex=0.6)

pred_plsr =predict(plsFit,ncomp=5,newdata=as.matrix(datos_validac[,2:6]))
sum((datos_validac[,7:11]-pred_plsr)^2)

# 6. Agrupamiento jerarquico
datos_escalados<-scale(datos)
d=dist(datos_escalados,method = "euclidian")
clus1<-hclust(d,method = "ward.D2")
subgrups <- cutree(clus1, k = 5)
table(datos$Poblacion,as.factor(subgrups))


#kmeans
res.hk <-hkmeans(datos_escalados, 5)
paleta<-c("#046e6c","#fbb29e","#80c1c5","#800080","#926343")
fviz_cluster(res.hk,ggtheme = theme_minimal(),palette=paleta)

paleta_ordenada<-c("#80c1c5","#046e6c","#926343","#fbb29e","#800080")
# dendrograma
fviz_dend(res.hk, cex = 0.6,  rect = TRUE, rect_fill = TRUE,palette = paleta_ordenada,rect_border = paleta_ordenada)
