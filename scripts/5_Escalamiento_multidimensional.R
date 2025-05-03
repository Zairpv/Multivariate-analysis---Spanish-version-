################################################################################
# Script: 1_Exploratory analyses for RF.R
# Author: Dra. Zaira Rosario Pérez-Vázquez
# Contact: zairpv@gmail.com
#
################################################################################


#1. LIBRERIAS ------------------------------------------------------------------
library(vegan)
library(ggplot2)

#2. CARGAR DATOS

datos_suelos<-read.csv("data/5_Datos_suelos.csv",header = T)
colSums(is.na(datos_suelos))
View(datos_suelos)
names(datos_suelos)

Landuses<-datos_suelos$land.use.fields
field<-datos_suelos$Field.

variables<-datos_suelos[,22:23]
names(variables)<-c("bacterial","fungal")

#PCR-time
?metaMDS
Realtime_PCR<-metaMDS(variables,distance = "bray",
             k=2,trymax = 50,autotransform = FALSE,
             wasscores=TRUE,noshare = FALSE)
Realtime_PCR

PCR_vars<-Realtime_PCR$species
View(PCR_vars)

PCR_samples<-data.frame(Realtime_PCR$points)
names(PCR_samples)
#Definir paletas similar a las de Kuramae et al. 2011
landuse_palette<-c("#000000","#9e5111","#f4c91e","#922a9b","#63b747","#ff7373")

plot1<-ggplot(PCR_samples,aes(PCR_samples[,1],PCR_samples[,2],color=Landuses,label=field))+
  geom_point(shape=15)+theme_bw()+theme_classic()+xlab("")+
  xlim(-0.6,1)+ylim(-0.6, 0.6)+
  ylab("Real-time PCR")+geom_vline(xintercept = 0) + geom_hline(yintercept=0)+
  ggtitle("NMDS")+scale_color_manual(values = landuse_palette)+
  geom_text(check_overlap = TRUE,size=2.8,hjust = 0, nudge_x = 0.03,nudge_y = 0.04)+
  annotate("text",x=0.75,y=0.6,label="Stress = 0.06613",size=3.2)
plot1

# Phylochips
View(datos_suelos)
names(datos_suelos)
PhyloChips<-datos_suelos[,24:42]  
View(PhyloChips)
names(PhyloChips)
NMDS_PhyloChips<-metaMDS(PhiloChips,distance = "bray",k=2,trymax = 50,
                 wascores = TRUE,autotransform = FALSE,noshare = FALSE)
NMDS_PhiloChips

PhyloChips_vars<-NMDS_PhiloChips$species
PhyloChips_sample<-data.frame(NMDS_PhiloChips$points)

plot2<-ggplot(PhyloChips_sample,aes(PhyloChips_sample[,1],PhyloChips_sample[,2],color=Landuses,label=field))+
  geom_point(shape=15)+theme_bw()+theme_classic()+xlab("")+
  xlim(-0.6,0.8)+ylim(-0.1, 0.1)+
  ylab("Phylochips")+geom_vline(xintercept = 0) + geom_hline(yintercept=0)+
  ggtitle("NMDS")+scale_color_manual(values = landuse_palette)+
  geom_text(check_overlap = TRUE,size=2.8,hjust = 0, nudge_x = 0.03,nudge_y = 0.04)+
  annotate("text",x=0.50,y=0.1,label="Stress = 0.0481",size=3.2)
plot2

