################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################

# 1. LIBRERIAS -----------------------------------------------------------------
library(psych) 
library(pracma)
library(corrplot)
library(PerformanceAnalytics)


# 2. CARGAR DATOS --------------------------------------------------------------
options(max.print = 10000)
datos_trigo<-read.csv("data/2_datos_trigo_t2.csv",header = TRUE)
datos_trigo
attach(datos_trigo)
View(datos_trigo)

# 3. TASKS ---------------------------------------------------------------------
# a) Resumen de la base de datos: tabla multivariada de medias y desviaciones estandar por variedad.
summary(datos_trigo)
describeBy(datos_trigo[1:7],group = datos_trigo$variedad, digits = 4) #resumen agrupado por variedad


# b)	Tabla de matrices de varianzas y covarianzas, asi como la de correlaciones de Pearson.
#matrices para todos los datos (indistinto de variedad
round(var(datos_trigo[1:7]),3)
round(cov(datos_trigo[1:7]),3)
round(cor(datos_trigo[1:7],method = "pearson"),3)

#matrices por variedad de trigo
variedad1<-datos_trigo[datos_trigo$variedad==1,]
variedad2<-datos_trigo[datos_trigo$variedad==2,]
variedad3<-datos_trigo[datos_trigo$variedad==3,]

round(var(variedad1[1:7]),3)
round(cov(variedad1[1:7]),3)
round(cor(variedad1[1:7],method = "pearson"),3)

round(var(variedad2[1:7]),3)
round(cov(variedad2[1:7]),3)
round(cor(variedad2[1:7],method = "pearson"),3)

round(var(variedad3[1:7]),3)
round(cov(variedad3[1:7]),3)
round(cor(variedad3[1:7],method = "pearson"),3)


## c)	Graficas boxplot para determinar diferencias entre variedades para cada variable de medici?n.
names(datos_trigo)

boxplot(area~variedad,ylab="?rea",names=c("Kama","Rosa","Canadian"))
boxplot(perimeter~variedad,ylab="Per?metro",names=c("Kama","Rosa","Canadian"))
boxplot(compactness~variedad,ylab="Compacidad",names=c("Kama","Rosa","Canadian"))
boxplot(length_kernel~variedad,ylab="Longitud del grano",names=c("Kama","Rosa","Canadian"))
boxplot(width_kernel~variedad,ylab="Ancho del grano",names=c("Kama","Rosa","Canadian"))
boxplot(asymmetry_coefficient~variedad,xlab="Variedades de trigo",ylab="Coeficiente de asimetria",names=c("Kama","Rosa","Canadian"))
boxplot(length_of_kernel_groove~variedad,xlab="Variedades de trigo",ylab="Longitud de la ranura del grano",names=c("Kama","Rosa","Canadian"))


## d)	Grafica de Andrews para determinar gr?ficamente si se distinguen las variedades.
trigo<-as.matrix(datos_trigo[1:7])
str(variedad)

?andrewsplot
par(mfrow=c(1,2),mar=c(5,4,2,1),cex=1)
andrewsplot(trigo,variedad,style="cart")
andrewsplot(trigo,variedad,style="pol")
dev.off()

## e)	Graficas de matrices de correlaciones.

?corrplot()
names(datos_trigo)
names(datos_trigo)<-c("A","P","C","Long_K","Ancho_K","Asimetria","Long_R","Variedad")
Correlaciones<-cor(datos_trigo[1:7],method = "pearson")
corrplot(Correlaciones,method = "circle")

?chart.Correlation()
chart.Correlation(datos_trigo[1:7],histogram = T,cex=0.4,method = "pearson")

pairs(datos_trigo[1:7])
