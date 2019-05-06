library(stats)
library(ggplot2)
library(tidyverse)
library(colorspace)

#bring in SNP Data
GWAS_GD <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Dendrogram/GD_292.csv")

GWAS_GD[1:10,1:10]
GD <- GWAS_GD[1:292,6:ncol(GWAS_GD)]



#bring in metadata from BLUP dataframe
AllData <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv", header = T)
metadata <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv", header = T)

colnames(AllData)
justMG <- AllData %>% filter(Day == "6") %>% select(Entry,MG)
new_GWAS_GD <- left_join(justMG,GWAS_GD, by="Entry")
MG2_GWAS_GD <- new_GWAS_GD %>% filter(MG == "2")
MG2_GWAS_GD[1:10,1:10]
GD <- MG2_GWAS_GD[1:115,4:ncol(MG2_GWAS_GD)]
GD %>% select(ss715585038,ss715592735,ss715592739,ss715592740,ss715592859,ss715608804,ss715609570,ss715625118)

GD <- GWAS_GD[1:292,4:ncol(GWAS_GD)]

MG2_metadata <- metadata %>% filter(Day == "6") %>% filter(MG == "2")
#set all clusters as factor
cols = c(1:ncol(MG2_metadata))
MG2_metadata[cols] <- lapply(MG2_metadata[cols], factor)

#Convert GD into matrix form 
geno = as.matrix(GD)
geno[1:10,1:10]

which(apply(geno,2,var)==0)
geno <- geno[ , apply(geno,2,var) !=0]
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
pca1_data <- cbind(pca1_loading, MG2_metadata)
str(pca1_data)
pca1_data[,c(1:2,41:42)]
colnames(pca1_data)
Country <- MG2_metadata %>% select(Country)
PCA <- as.matrix(cbind(Country,pca1))
dt <- MG2_metadata %>% select(Stem.)
PCA <- as.matrix(cbind(dt,pca1))
#write.csv(PCA,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/PCA_nov7.csv", row.names = T)
#PCA <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/PCA_nov7.csv")


# make figure colored by pca
setwd('C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/')
tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/PCA_MG2_dt_April1.tiff",compression = "lzw", width = 6, height = 5, units = 'in', res = 500)
ggplot(pca1_data, aes(x = PC1, y = PC2, color = Stem.Termination)) +
  geom_point(alpha=0.99,size=2.5) +
  #stat_ellipse(type = "norm", level = 0.90, linetype = 1)+
  labs(x = c(pca1_data$sdev[1],"PC1 (12.6%)"), y = c(pca1_data$sdev[2], "PC2 (7.2%)"))+
  theme_classic() +
  scale_color_manual(values=c("magenta","orange2","#3399FF","black","magenta","blue"))+
  labs(color=expression(atop('Stem', 'Termination'))) +
  theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/PCA_MG2_diversity_April1.tiff",compression = "lzw", width = 6, height = 5, units = 'in', res = 500)
ggplot(pca1_data, aes(x = PC1, y = PC2, color = Diversity)) +
  geom_point(alpha=0.99,size=2.5) +
  #stat_ellipse(type = "norm", level = 0.90, linetype = 1)+
  labs(x = c(pca1_data$sdev[1],"PC1 (12.6%)"), y = c(pca1_data$sdev[2], "PC2 (7.2%)"))+
  theme_classic() +
  scale_color_manual(values=c("magenta","orange2","#3399FF","black","magenta","blue"))+
  labs(color=expression(atop('', 'Diversity'))) +
  theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))
dev.off()