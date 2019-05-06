library(stats)
library(ggplot2)
library(tidyverse)
library(colorspace)

#bring in SNP Data
GWAS_GD <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Dendrogram/GD_292.csv")
head(GWAS_GD)
GD <- GWAS_GD[1:292,6:ncol(GWAS_GD)]

#bring in metadata from BLUP dataframe
AllData <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv", header = T)
colnames(AllData)
AllData %>% select(Country) %>% table()
metadata <- subset(AllData, AllData$Day == "6")

Country <- metadata$Country
Country

#set all clusters as factor
cols = c(1:ncol(metadata))
metadata[cols] <- lapply(metadata[cols], factor)

#Convert GD into matrix form 
geno = as.matrix(GD)
geno[1:10,1:10]

################################################################################           
######################   PCA of Genotypic Information  #########################
################################################################################       

# PCA with function prcomp
#pca1 = prcomp(GD[,2:ncol(GD)], scale. = TRUE)
pca1 <- prcomp(geno, scale. = TRUE) #Performs a principal components analysis on the given data matrix and returns the results as an object of class prcomp
summary(pca1)

# loadings
pca1_loading =as.data.frame(pca1$x) #the value of the rotated data (the centred data multiplied by the rotation matrix) 

# add cluster info to pca
#write.csv(pca1_loading,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/PCA.csv", row.names = T)

#####################################################################################
#percent variance explained
summary(pca1)
#####################################################################################
pca1 <- as.data.frame(pca1$x)

#Bind metadata with PCA data for plotting
pca1_data <- cbind(metadata, pca1_loading)
str(metadata)
pca1_data[,c(1:2,41:42)]


PCA <- as.matrix(pca1)
#write.csv(PCA,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/PCA_nov7.csv", row.names = T)
#PCA <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/PCA_nov7.csv")
Country
# make figure colored by pca
setwd('C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/')
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/PCA_Country_March19.tiff",compression = "lzw", width = 6, height = 5, units = 'in', res = 500)
ggplot(pca1_loading, aes(x = PC1, y = PC2, color = Country)) +
  geom_point(alpha=0.99,size=2.5) +
  #stat_ellipse(type = "norm", level = 0.90, linetype = 1)+
  labs(x = c(pca1_loading$sdev[1],"PC1 (11.3 %)"), y = c(pca1_loading$sdev[2], "PC2 (6.2 %)"), color="Country")+
  theme_classic()+
  scale_color_manual(values=c("red","green2","cyan","black","magenta","blue"))+
  theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))
dev.off()
