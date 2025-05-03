################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################


#Conjunto de datos
rm(list=ls())
datos<-read.csv("data/datos problema1.csv",header=TRUE)
View(datos)
options(max.print = 100000)

#Librerias
library(dplyr)
library(psych)
library(MVN)
library(mvShapiroTest)
library(pracma)
library(andrews)
library(FactoMineR)
library(psych)
library(factoextra)
library(multiDimBio)
library(candisc)
library(randomForest)
library(caret)
library(BiocManager)
library(MASS)
library(e1071)
library(nnet)
library(NeuralNetTools)
library(rpart)




###1. Analisis exploratorio de los datos espectrales

##Ordenar los datos por clase de suelo
datos_clases<-arrange(datos,datos$class)
View(datos_clases)

##Clasificaci?n en bandas
banda1<-data.frame(datos_clases$A1,datos_clases$B1,datos_clases$C1,datos_clases$D1,datos_clases$E1,datos_clases$F1,datos_clases$G1,datos_clases$H1,datos_clases$I1,datos_clases$class)
banda2<-data.frame(datos_clases$A2,datos_clases$B2,datos_clases$C2,datos_clases$D2,datos_clases$E2,datos_clases$F2,datos_clases$G2,datos_clases$H2,datos_clases$I2,datos_clases$class)
banda3<-data.frame(datos_clases$A3,datos_clases$B3,datos_clases$C3,datos_clases$D3,datos_clases$E3,datos_clases$F3,datos_clases$G3,datos_clases$H3,datos_clases$I3,datos_clases$class)
banda4<-data.frame(datos_clases$A4,datos_clases$B4,datos_clases$C4,datos_clases$D4,datos_clases$E4,datos_clases$F4,datos_clases$G4,datos_clases$H4,datos_clases$I4,datos_clases$class)
View(banda1)
View(banda2)
View(banda3)
View(banda4)
banda1$datos_clases.class<-as.factor(banda1$datos_clases.class)
banda2$datos_clases.class<-as.factor(banda2$datos_clases.class)
banda3$datos_clases.class<-as.factor(banda3$datos_clases.class)
banda4$datos_clases.class<-as.factor(banda4$datos_clases.class)
names(banda1)<-c("A1","B1","C1","D1","E1","F1","G1","H1","I1","Clase")
names(banda2)<-c("A2","B2","C2","D2","E2","F2","G2","H2","I2","Clase")
names(banda3)<-c("A3","B3","C3","D3","E3","F3","G3","H3","I3","Clase")
names(banda4)<-c("A4","B4","C4","D4","E4","F4","G4","H4","I4","Clase")

##Analisis exploratorio por clase de suelo

#Banda 1
summary(banda1[1:1072,])        #Clase 1
summary(banda1[1073:1551,])     #Clase 2
summary(banda1[1552:2512,])     #Clase 3
sd(banda1[1:1072,]$A1)
sd(banda1[1:1072,]$B1)
sd(banda1[1:1072,]$C1)
sd(banda1[1:1072,]$D1)
sd(banda1[1:1072,]$E1)
sd(banda1[1:1072,]$F1)
sd(banda1[1:1072,]$G1)
sd(banda1[1:1072,]$H1)
sd(banda1[1:1072,]$I1)
sd(banda1[1073:1551,]$A1)
sd(banda1[1073:1551,]$B1)
sd(banda1[1073:1551,]$C1)
sd(banda1[1073:1551,]$D1)
sd(banda1[1073:1551,]$E1)
sd(banda1[1073:1551,]$F1)
sd(banda1[1073:1551,]$G1)
sd(banda1[1073:1551,]$H1)
sd(banda1[1073:1551,]$I1)
sd(banda1[1552:2512,]$A1)
sd(banda1[1552:2512,]$B1)
sd(banda1[1552:2512,]$C1)
sd(banda1[1552:2512,]$D1)
sd(banda1[1552:2512,]$E1)
sd(banda1[1552:2512,]$F1)
sd(banda1[1552:2512,]$G1)
sd(banda1[1552:2512,]$H1)
sd(banda1[1552:2512,]$I1)
#Banda 2
summary(banda2[1:1072,])        #Clase 1
summary(banda2[1073:1551,])     #Clase 2
summary(banda2[1552:2512,])     #Clase 3
sd(banda2[1:1072,]$A2)
sd(banda2[1:1072,]$B2)
sd(banda2[1:1072,]$C2)
sd(banda2[1:1072,]$D2)
sd(banda2[1:1072,]$E2)
sd(banda2[1:1072,]$F2)
sd(banda2[1:1072,]$G2)
sd(banda2[1:1072,]$H2)
sd(banda2[1:1072,]$I2)
sd(banda2[1073:1551,]$A2)
sd(banda2[1073:1551,]$B2)
sd(banda2[1073:1551,]$C2)
sd(banda2[1073:1551,]$D2)
sd(banda2[1073:1551,]$E2)
sd(banda2[1073:1551,]$F2)
sd(banda2[1073:1551,]$G2)
sd(banda2[1073:1551,]$H2)
sd(banda2[1073:1551,]$I2)
sd(banda2[1552:2512,]$A2)
sd(banda2[1552:2512,]$B2)
sd(banda2[1552:2512,]$C2)
sd(banda2[1552:2512,]$D2)
sd(banda2[1552:2512,]$E2)
sd(banda2[1552:2512,]$F2)
sd(banda2[1552:2512,]$G2)
sd(banda2[1552:2512,]$H2)
sd(banda2[1552:2512,]$I2)
#Banda 3
summary(banda3[1:1072,])        #Clase 1
summary(banda3[1073:1551,])     #Clase 2
summary(banda3[1552:2512,])     #Clase 3
sd(banda3[1:1072,]$A3)
sd(banda3[1:1072,]$B3)
sd(banda3[1:1072,]$C3)
sd(banda3[1:1072,]$D3)
sd(banda3[1:1072,]$E3)
sd(banda3[1:1072,]$F3)
sd(banda3[1:1072,]$G3)
sd(banda3[1:1072,]$H3)
sd(banda3[1:1072,]$I3)
sd(banda3[1073:1551,]$A3)
sd(banda3[1073:1551,]$B3)
sd(banda3[1073:1551,]$C3)
sd(banda3[1073:1551,]$D3)
sd(banda3[1073:1551,]$E3)
sd(banda3[1073:1551,]$F3)
sd(banda3[1073:1551,]$G3)
sd(banda3[1073:1551,]$H3)
sd(banda3[1073:1551,]$I3)
sd(banda3[1552:2512,]$A3)
sd(banda3[1552:2512,]$B3)
sd(banda3[1552:2512,]$C3)
sd(banda3[1552:2512,]$D3)
sd(banda3[1552:2512,]$E3)
sd(banda3[1552:2512,]$F3)
sd(banda3[1552:2512,]$G3)
sd(banda3[1552:2512,]$H3)
sd(banda3[1552:2512,]$I3)
#Banda 4
summary(banda4[1:1072,])        #Clase 1
summary(banda4[1073:1551,])     #Clase 2
summary(banda4[1552:2512,])     #Clase 3
sd(banda4[1:1072,]$A4)
sd(banda4[1:1072,]$B4)
sd(banda4[1:1072,]$C4)
sd(banda4[1:1072,]$D4)
sd(banda4[1:1072,]$E4)
sd(banda4[1:1072,]$F4)
sd(banda4[1:1072,]$G4)
sd(banda4[1:1072,]$H4)
sd(banda4[1:1072,]$I4)
sd(banda4[1073:1551,]$A4)
sd(banda4[1073:1551,]$B4)
sd(banda4[1073:1551,]$C4)
sd(banda4[1073:1551,]$D4)
sd(banda4[1073:1551,]$E4)
sd(banda4[1073:1551,]$F4)
sd(banda4[1073:1551,]$G4)
sd(banda4[1073:1551,]$H4)
sd(banda4[1073:1551,]$I4)
sd(banda4[1552:2512,]$A4)
sd(banda4[1552:2512,]$B4)
sd(banda4[1552:2512,]$C4)
sd(banda4[1552:2512,]$D4)
sd(banda4[1552:2512,]$E4)
sd(banda4[1552:2512,]$F4)
sd(banda4[1552:2512,]$G4)
sd(banda4[1552:2512,]$H4)
sd(banda4[1552:2512,]$I4)

##Diagramas de boxplot

#Banda 1
png("P1_1.BoxplotB1.png",width = 710,height = 560)
par(mfrow=c(3,3),cex=0.9,mar=c(1.8,1.9,1.9,1.6))
boxplot(banda1$A1~banda1$Clase,main="A1")
boxplot(banda1$B1~banda1$Clase,main="B1")
boxplot(banda1$C1~banda1$Clase,main="C1")
boxplot(banda1$D1~banda1$Clase,main="D1")
boxplot(banda1$E1~banda1$Clase,main="E1")
boxplot(banda1$F1~banda1$Clase,main="F1")
boxplot(banda1$G1~banda1$Clase,main="G1")
boxplot(banda1$H1~banda1$Clase,main="H1")
boxplot(banda1$I1~banda1$Clase,main="I1")
dev.off()

#Banda2
png("P1_2.BoxplotB2.png",width = 710,height = 560)
par(mfrow=c(3,3),cex=0.9,mar=c(1.8,1.9,1.9,1.6))
boxplot(banda2$A2~banda2$Clase,main="A2")
boxplot(banda2$B2~banda2$Clase,main="B2")
boxplot(banda2$C2~banda2$Clase,main="C2")
boxplot(banda2$D2~banda2$Clase,main="D2")
boxplot(banda2$E2~banda2$Clase,main="E2")
boxplot(banda2$F2~banda2$Clase,main="F2")
boxplot(banda2$G2~banda2$Clase,main="G2")
boxplot(banda2$H2~banda2$Clase,main="H2")
boxplot(banda2$I2~banda2$Clase,main="I2")
dev.off()

#Banda3
png("P1_3.BoxplotB3.png",width = 710,height = 560)
par(mfrow=c(3,3),cex=0.9,mar=c(1.8,1.9,1.9,1.6))
boxplot(banda3$A3~banda3$Clase,main="A3")
boxplot(banda3$B3~banda3$Clase,main="B3")
boxplot(banda3$C3~banda3$Clase,main="C3")
boxplot(banda3$D3~banda3$Clase,main="D3")
boxplot(banda3$E3~banda3$Clase,main="E3")
boxplot(banda3$F3~banda3$Clase,main="F3")
boxplot(banda3$G3~banda3$Clase,main="G3")
boxplot(banda3$H3~banda3$Clase,main="H3")
boxplot(banda3$I3~banda3$Clase,main="I3")
dev.off()

#Banda 4
png("P1_4.BoxplotB4.png",width = 710,height = 560)
par(mfrow=c(3,3),cex=0.9,mar=c(1.8,1.9,1.9,1.6))
boxplot(banda4$A4~banda4$Clase,main="A4")
boxplot(banda4$B4~banda4$Clase,main="B4")
boxplot(banda4$C4~banda4$Clase,main="C4")
boxplot(banda4$D4~banda4$Clase,main="D4")
boxplot(banda4$E4~banda4$Clase,main="E4")
boxplot(banda4$F4~banda4$Clase,main="F4")
boxplot(banda4$G4~banda4$Clase,main="G4")
boxplot(banda4$H4~banda4$Clase,main="H4")
boxplot(banda4$I4~banda4$Clase,main="I4")
dev.off()

##Diagramas de dispersi?n, histogramas y correlacion. 
#Banda1
png("P1_5.DispersionB1.png",width = 710,height = 560)
pairs.panels(banda1,pch = 0.1)
dev.off()
#Banda2
png("P1_6.DispersionB2.png",width = 710,height = 560)
pairs.panels(banda2,pch = 0.1)
dev.off()
#Banda3
png("P1_7.DispersionB3.png",width = 710,height = 560)
pairs.panels(banda3,pch = 0.1)
dev.off()
#Banda4
png("P1_8.DispersionB4.png",width = 710,height = 560)
pairs.panels(banda4,pch = 0.1)
dev.off()
pairs

#Gr?fico de Andrews tipo pol
?andrewsplot()
#Banda1
andrewsplot(as.matrix(banda1[1:9]),banda1[,10], style = "pol")
title(main=NULL,sub="Banda1")
#Banda2
andrewsplot(as.matrix(banda2[1:9]),banda2[,10], style = "pol")
title(main=NULL,sub="Banda2")
#Banda3
andrewsplot(as.matrix(banda3[1:9]),banda3[,10], style = "pol")
title(main=NULL,sub="Banda3")
#Banda4
andrewsplot(as.matrix(banda4[1:9]),banda4[,10], style = "pol")
title(main=NULL,sub="Banda4")

#Gr?fico de Andrews tipo cart
#Banda1
andrewsplot(as.matrix(banda1[1:9]),banda1[,10], style = "cart")
title(main=NULL,sub="Banda1")
#Banda2
andrewsplot(as.matrix(banda2[1:9]),banda2[,10], style = "cart")
title(main=NULL,sub="Banda2")
#Banda3
andrewsplot(as.matrix(banda3[1:9]),banda3[,10], style = "cart")
title(main=NULL,sub="Banda3")
#Banda4
andrewsplot(as.matrix(banda4[1:9]),banda4[,10], style = "cart")
title(main=NULL,sub="Banda4")


###2. Normalidad multivariada

#Normalidad multivariada por clase de cobertura en las 4 bandas evaluadas
clase1<-datos_clases[1:1072,]
clase2<-datos_clases[1073:1551,]
clase3<-datos_clases[1552:2512,]
(View(clase1))

#Clase1
Norm1<-mvn(data = clase1[,1:36],mvnTest = "mardia")
Norm1$multivariateNormality
Norm1<-mvn(data = clase1[,1:36],mvnTest = "hz")
Norm1$multivariateNormality
Norm1<-mvn(data = clase1[,1:36],mvnTest = "royston")
Norm1$multivariateNormality
Norm1<-mvn(data = clase1[,1:36],mvnTest = "dh")
Norm1$multivariateNormality
Norm1<-mvn(data = clase1[,1:36],mvnTest = "energy")
Norm1$multivariateNormality

#Clase2
Norm2<-mvn(data = clase2[,1:36],mvnTest = "mardia")
Norm2$multivariateNormality
Norm2<-mvn(data = clase2[,1:36],mvnTest = "hz")
Norm2$multivariateNormality
Norm2<-mvn(data = clase2[,1:36],mvnTest = "royston")
Norm2$multivariateNormality
Norm2<-mvn(data = clase2[,1:36],mvnTest = "dh")
Norm2$multivariateNormality
Norm2<-mvn(data = clase2[,1:36],mvnTest = "energy")
Norm2$multivariateNormality

#Clase3
Norm3<-mvn(data = clase3[,1:36],mvnTest = "mardia")
Norm3$multivariateNormality
Norm3<-mvn(data = clase3[,1:36],mvnTest = "hz")
Norm3$multivariateNormality
Norm3<-mvn(data = clase3[,1:36],mvnTest = "royston")
Norm3$multivariateNormality
Norm3<-mvn(data = clase3[,1:36],mvnTest = "dh")
Norm3$multivariateNormality
Norm3<-mvn(data = clase3[,1:36],mvnTest = "energy")
Norm3$multivariateNormality

#Normalidad multivariada por clase de cobertura en cada bandas evaluadas
#Banda1 - Clase 1-3
View(banda1[1:1072,1:9])
Norm_B1C1<-mvn(data = banda1[1:1072,1:9],mvnTest = "mardia")
Norm_B1C1
Norm_B1C2<-mvn(data = banda1[1073:1551,1:9],mvnTest = "mardia")
Norm_B1C2
Norm_B1C3<-mvn(data = banda1[1552:2512,1:9],mvnTest = "mardia")
Norm_B1C3

#Banda2 - Clase 1-3
View(banda2[1:1072,1:9])
Norm_B2C1<-mvn(data = banda2[1:1072,1:9],mvnTest = "mardia")
Norm_B2C1
Norm_B2C2<-mvn(data = banda2[1073:1551,1:9],mvnTest = "mardia")
Norm_B2C2
Norm_B2C3<-mvn(data = banda2[1552:2512,1:9],mvnTest = "mardia")
Norm_B2C3

#Banda3 - Clase 1-3
View(banda1[1:1072,1:9])
Norm_B3C1<-mvn(data = banda3[1:1072,1:9],mvnTest = "mardia")
Norm_B3C1
Norm_B3C2<-mvn(data = banda3[1073:1551,1:9],mvnTest = "mardia")
Norm_B3C2
Norm_B3C3<-mvn(data = banda3[1552:2512,1:9],mvnTest = "mardia")
Norm_B3C3

#Banda4 - Clase 1-3
View(banda1[1:1072,1:9])
Norm_B4C1<-mvn(data = banda4[1:1072,1:9],mvnTest = "mardia")
Norm_B4C1
Norm_B4C2<-mvn(data = banda4[1073:1551,1:9],mvnTest = "mardia")
Norm_B4C2
Norm_B4C3<-mvn(data = banda4[1552:2512,1:9],mvnTest = "mardia")
Norm_B4C3

### 3. Reducci?n de dimensiones por PCA

#Prueba de Bartlett  (El valor de n representa el numero de observaciones totales por clase)
bartlett.test(clase1[,1:36],n=1072) 
bartlett.test(clase2[,1:36],n=479)
bartlett.test(clase3[,1:36],n=961)

View(datos_clases[,1:36])
?prcomp
PCA_multibanda<-prcomp(datos_clases[,1:36],center = TRUE,scale. = FALSE)
summary(PCA_multibanda)
biplot(PCA_multibanda,scale = 0)

#Considerando la extracci?n de cuatro componentes
PCA_multibanda4<-PCA(datos_clases[,1:36],scale=FALSE,ncp=4)

fviz_screeplot(PCA_multibanda4,addlabels=FALSE,geom="line")
plot(PCA_multibanda4,choix = "ind",cex=0.7)
plot.PCA(PCA_multibanda4,axes=c(1,2),choix = "var",habillage = 2,cex=0.7,ellipse = TRUE)
paleta<-c("#451460","#56b893","#a9c12a")
fviz_pca_var(PCA_multibanda4,col.var = "cos2",gradient.cols=paleta,repel=TRUE)

clases_cobertura<-as.factor(datos_clases[,37])

### 4. Diferencias entre grupos MANOVA
model<-lm(as.matrix(datos_clases[,1:36])~as.factor(clases_cobertura))

manova
Manova1<-manova(model)
summary(Manova1)

### 5. M?todos de aprendizaje autom?tico
set.seed(100100)

#Datos de entrenamiento y validaci?n
datos_clases$class<-as.factor(datos_clases$class)

Y_resp<-datos_clases[,37]
View(Y_resp)
X_pred <- datos_clases[,1:36]
View(X_pred)
particion<-createDataPartition(Y_resp, p=0.80, list=FALSE, times=1) 
training<-datos_clases[particion,]
View(training)
validation<-datos_clases[-particion,]  
View(validation)

#Discriminaci?n canonica
DiscrimCan<-candisc(model, data = training, ndim =2)
DiscrimCan
DiscrimCan$coeffs.std
DiscrimCan$structure
plot(DiscrimCan) 

#LDA
LDA1<-lda(training$class~.,data = training)
LDA1
LDA_pred <-predict(LDA1, validation)
confusionMatrix(LDA_pred$class, validation$class)

#Vectorial support
SVM1<- svm(training$class~., data=training)
summary(SVM1)
SVM1$rho
SVM_pred <- predict(SVM1, validation)
confusionMatrix(SVM_pred, validation$class)

#Decision trees
trees=rpart(training$class~., data=training)
rpart::plotcp(trees)
rpart.plot(trees)

#Ramdon Forest
RF_1<- randomForest(training$class~., data = training, mtry=10, ntree=1000)
RF_1
plot(RF_1)
varImpPlot(RF_1)
RF_pred <- predict(RF_1, validation)
confusionMatrix(RF_pred, validation$class)

#Redes neuronales
NN1=nnet(training$class~., data = training,size=10, decay=0.1, maxit=1000)
NN1
NN1_pred= data.frame(predict(NN1, validation), class=predict(NN1, validation, type="class"))
View(training$class)
plotnet(NN1)
confusionMatrix(NN1_pred$class, validation$class)
