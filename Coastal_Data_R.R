# clear work space
rm(list=ls()) 

#returns working directory 
getwd() 

#sets working directory 
setwd("C:/Users/willo/OneDrive/Documents/Bio/Coastal 2022")

#loads libraries needed 
library(readxl) 
library("writexl")
library(dplyr)
library(tidyverse)
library("vegan")
library(ggplot2)
library(plyr)

library(FSA)

#Load excel data
d <- read.csv("Coastal_datasheet.csv")
data = data.frame(d)

#Reorganizing data so Blue sharks can be blue bc I couldnt figure out how to assign a variable a specific color
data$Species <- factor(data$Species , levels=c("Smooth Dog", "Mako", "Blue"))
dataf <- data[complete.cases(data), ]

########  BoxPlots  #########
#Pec Aspect Ratio boxplot
dataf %>%
ggplot(aes(x = Species, y = Pectoral.Aspect.Ratio, color = Species)) + 
  geom_boxplot() + theme_classic() + xlab("Species") + ylab("Pectoral Fin Aspect Ratio (h^2/SA)") 

kruskal.test(Pectoral.Aspect.Ratio ~ Species, data = dataf)
dunnTest(Pectoral.Aspect.Ratio ~ Species, data = dataf, method = "bh")

#Doubles Blue shark sample size for test
#dataf2 <- rbind(dataf, dataf[2:3,])


#Caudal Aspect Ratio boxplot
dataf %>%
  ggplot(aes(x = Species, y = Caudal.Aspect.Ratio, color = Species)) + 
  geom_boxplot() + theme_classic() + xlab("Species") + ylab("Caudal Fin Aspect Ratio (h^2/SA)")

kruskal.test(Caudal.Aspect.Ratio ~ Species, data = dataf)
dunnTest(Caudal.Aspect.Ratio ~ Species, data = dataf, method = "bh")


#Gill slit heigtht
dataf %>%
  ggplot(aes(x = Species, y = Gill.Slit.Height...PCL, color = Species)) + 
  geom_boxplot() + theme_classic() + xlab("Species") + ylab("Gill Slit Height (cm)")

kruskal.test(Gill.Slit.Height...PCL ~ Species, data = data)
dunnTest(Gill.Slit.Height...PCL ~ Species, data = dataf, method = "bh")


#Snout to gill slit
dataf %>%
  ggplot(aes(x = Species, y = Snout.to.Gill.Slit..PCL, color = Species)) + 
  geom_boxplot()+ theme_classic() + xlab("Species") + ylab("Distance Snout to Gill Slit (cm)")

kruskal.test(Snout.to.Gill.Slit..PCL ~ Species, data = dataf)


#Pec Aspect Ratio vs Total Length line graph 
dataf %>%
  ggplot(aes(x = Pectoral.Aspect.Ratio, y = Pre.Caudal.Length, color = Species)) + 
  geom_point() + theme_classic()


#Girth Boxplots 
dataf %>%
  ggplot(aes(x = Species, y = PPG_PCL, color = Species)) + 
  geom_boxplot()+ theme_classic() + xlab("Species") + ylab("Pre-Pectoral Girth (cm)")

kruskal.test(PPG_PCL ~ Species, data = dataf)
dunnTest(PPG_PCL ~ Species, data = dataf, method = "bh")

dataf %>%
  ggplot(aes(x = Species, y = PoPG_PCL, color = Species)) + 
  geom_boxplot()+ theme_classic() + xlab("Species") + ylab("Post-Pectoral Girth (cm)")

kruskal.test(PoPG_PCL ~ Species, data = data)
dunnTest(PoPG_PCL ~ Species, data = dataf, method = "bh")

dataf %>%
  ggplot(aes(x = Species, y = PFDG_PCL, color = Species)) + 
  geom_boxplot()+ theme_classic() + xlab("Species") + ylab("Post First Dorsal Fin Girth (cm)")

kruskal.test(PFDG_PCL ~ Species, data = dataf)
dunnTest(PFDG_PCL ~ Species, data = dataf, method = "bh")


#Pec Fin SA as proportion of Pre Caudal Length 
dataf %>%
  ggplot(aes(x = Pre.Caudal.Length, y = Pectoral.SA.Avg, color = Species)) + 
  geom_point()+ theme_classic() + xlab("Pre Caudal Length (cm)") + ylab("Pectoral Fin Surface Area (cm)")

kruskal.test(Pre.Caudal.Length ~ Pectoral.SA.Avg, data = data)

dataf$PFSA_PCL <- c(dataf$Pectoral.SA.Avg/dataf$Pre.Caudal.Length)

dataf %>%
  ggplot(aes(x = Species, y = PFSA_PCL, color = Species)) + 
  geom_boxplot()+ theme_classic() + xlab("Species") + ylab("Pectoral Fin SA / Pre Caudal Length (cm)")

kruskal.test(PFSA_PCL ~ Species, data = dataf)
dunnTest(PFSA_PCL ~ Species, data = dataf, method = "bh")


#Conical stuff 
d2 <- read.csv("Coastal_field_measurment_datasheet2.csv")
data2 = data.frame(d2)

data2$Species <- factor(data2$Species , levels=c("Smooth Dog", "Mako", "Blue"))

data2 %>%
  ggplot(aes(x = Species, y = Conical, color = Species)) + 
  geom_boxplot()+ theme_classic() + xlab("Species") + ylab("Snout Angle")

kruskal.test(Conical ~ Species, data = data2)
dunnTest(Conical ~ Species, data = data2, method = "bh")


#Lunate
lunate <- read.csv("Coastal_lunate.csv")
lunate = data.frame(lunate)

lunate$Species <- factor(lunate$Species , levels=c("Smooth Dog", "Mako", "Blue"))

lunate %>%
  ggplot(aes(x = Lunate1, y = Lunate2, color = Species)) + 
  geom_point()+ theme_classic() + xlab("Distance Mid to Top Caudal") + ylab("Distance Mid to Bottom Caudal")

lunate %>%
  ggplot(aes(x = Species, y = Lunate_Ratio, color = Species)) + 
  geom_boxplot()+ theme_classic() + xlab("Distance Mid to Top Caudal") + ylab("Distance Mid to Bottom Caudal")

kruskal.test(Lunate_Ratio ~ Species, data = lunate)
dunnTest(Lunate_Ratio ~ Species, data = lunate, method = "bh")



########  PCA  ##########
#Make sure dataframe as no NA values 

#Remove values I don't want in a PCA
data_PCA <-  subset(dataf, select = c(PoPG_PCL, PFDG_PCL, Gill.Slit.Height...PCL, Pectoral.Aspect.Ratio, Caudal.Aspect.Ratio, Conical, Lunate1, Lunate2, PFSA_PCL) )

#Creates a frame that can be used for PCAs
data.pca <- prcomp(data_PCA[,c(1:9)], center = TRUE,scale. = TRUE)

#load libraries
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(data.pca)

#Making meta data
data.species <- c(rep("Mako"), rep("Blue"), rep("Blue"), rep("Smooth Dog"), rep("Smooth Dog"), rep("Smooth Dog"), rep("Smooth Dog"), rep("Mako"), rep("Mako"), rep("Mako"), rep("Mako"), rep("Smooth Dog"), rep("Smooth Dog"))

#plotting
ggbiplot(data.pca,ellipse=TRUE,  labels=rownames(data.pca), groups=data.species) + 
  xlim(-2.0,3.0) + ylim(-2.5,3.0) +
  theme_minimal()+
  scale_colour_manual(name="Origin", values= c("blue3", "green3", "red3"))
                      
                      

##TO DO LIST:
#Max Lift (2), Burst speed (1), Reduced drag (3)

##TO DO

