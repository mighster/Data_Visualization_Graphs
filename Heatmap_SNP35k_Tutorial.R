#Parallel computing
library(snow)
library(doSNOW)
library(parallel)
detectCores()
cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)
library(dplyr)

#Input genotypic data set
GD <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Genotypic Data Stuff/GWAS_GD.txt",header = T)
GD[1:10,1:10]
GD <- GD[,-c(1:3)] #remove names, metadata
GD <- as.matrix(GD) #convert to matrix

#############################################################################################################
#############################   Calculate Genetic Distance and Create Dendrogram ############################
#############################################################################################################

#Compute genetic distance using NAM package
library(NAM)
gdist1 = Gdist(GD, method = 1) #This function computes measures of genetic distances between populations using a genpop object. 

#Create dendrogram
library(dendextend)
geno_dend <- as.dendrogram(hclust(gdist1,method='ward.D'))
plot(geno_dend)
#Customize dendrogram, set branches to specified width 
geno_dend <- set(geno_dend, "branches_lwd", 2)
#set branches to specified color
cols_branches <-c("yellow2","magenta","red","orange","blue","black","cyan","green3")
#Customize dendrogram
geno_dend <- color_branches(geno_dend, k=8, col = cols_branches)
plot(geno_dend)

#############################################################################################################
##################################   Generate Heatmap from Phenotypic Data   ################################
#############################################################################################################

#read in phenotypic data
AllData <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/GWAS/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall.csv")

#split data into df Days
Day6_data <- subset(AllData, AllData$Day == "6")
Day9_data <- subset(AllData, AllData$Day == "9")
Day12_data <- subset(AllData, AllData$Day == "12")
colnames(Day9_data)

#Select the phenotypic data to showcase in the heatmap
pheno_traits <- Day9_data %>% dplyr::select(TRL,PRL,WID,CVA,LRB,VOL,LED,RHZO,TRLUpper,Root_weight)
pheno_traits[1:10,1:10]
#Scale the phenotypic data to normalize it to -4, +4
pheno_traits_scaled <- scale(as.matrix(pheno_traits))
pheno_traits_scaled[1:10,1:10]
#check your data to ensure there are not large outliers causing skews
summary(pheno_traits_scaled)

#############################################################################################################
###########################  Calculate Phenotypic Distance and Create Dendrogram ############################
#############################################################################################################
pheno_dend <- as.dendrogram(hclust(as.dist(1-cor(pheno_traits_scaled, method="spearman")), method="complete"))
plot(pheno_dend)
#Customize dendrogram, set branches to specified width 
pheno_dend <- set(pheno_dend, "branches_lwd", 2)
#set branches to specified color
cols_branches <-c("green3","magenta","red","orange","blue")
#Customize dendrogram
pheno_dend <- color_branches(pheno_dend, k=5, col = cols_branches)
plot(pheno_dend)

#Select HEATMAP Colours
library(gplots)
mycol_blueorange <- colorpanel(50,"White","yellow", "red")

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day9_heatmap_13Traits_March19.tiff",compression = "lzw", width = 13, height = 15, units = 'in', res = 500)
heatmap.2(x=pheno_traits_scaled, Rowv=geno_dend, Colv=pheno_dend, col=mycol_blueorange, scale="none", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day9_data$Entry,
          keysize = 1, main = "Root Traits at Day 9")
dev.off()