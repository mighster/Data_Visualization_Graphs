library(dplyr)
library(adegenet) #package devoted to the multivariate analysis of genetic markers data. (df2genind)
library(NAM) #nested association mapping - Alencar Xavier (Gdist)
library(dendextend)
library(circlize)
#Parallel computing
library(snow)
library(doSNOW)
library(parallel)
detectCores()
cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)

https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
https://www.r-bloggers.com/hierarchical-clustering-in-r-2/

#Input genotypic data set
GD <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Genotypic Data Stuff/GWAS_GD.txt",header = T)
GD[1:10,1:10]

#Compute genetic distance using NAM package
gdist1 = Gdist(GD, method = 1) #This function computes measures of genetic distances between populations using a genpop object. 

# How many clusters?
# 1. Make genind object to be used in further analysis. 
#    df2genind converts a data.frame into a genind object. (adegenet)
obj <- df2genind(GD, ploidy=2, sep = '/t') 
# 2. try different values of k (interactive) using kmeans
grp <- find.clusters(obj, max.n=20, n.pca=200, scale=FALSE) 
plot(grp)

# Clustering Method 1: Nei's distance (not Euclidean) (stats package)
hier_cluster_m1 <- (hclust(gdist1,method='ward.D')) #Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.

#Cuts a tree, e.g., as resulting from hclust, into several group
cutree.8 <- cutree(hier_cluster_m1, k = 8)
summary(as.factor(cutree(hier_cluster_m1,k = 8)))
  
# Create dendrogram
dend <- as.dendrogram(hclust(gdist1,method='ward.D'))
# plot dendrogram
plot(dend)

#############################################################################################################
############################################   Import Labels ################################################
#############################################################################################################
data_labels <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")
#make everything a factor
data_labels[] <- lapply(data_labels[], factor)

#split data into df Days
Day6_data_labels <- subset(data_labels, data_labels$Day == "6")
Day9_data_labels <- subset(data_labels, data_labels$Day == "9")
Day12_data_labels <- subset(data_labels, data_labels$Day == "12")
colnames(Day9_data_labels)

#############################################################################################################
##################################   Compare Cluster Methods ################################################
#############################################################################################################

# Change the size of the labels
dend <- set(dend, "labels_cex", 0.5)

# Change the thickness of the branches
dend <- set(dend, "branches_lwd", 2)

# get some colors
cols_branches <- c("red","black","yellow2","orange","magenta","blue","green3","cyan")

#Set the branches to 8 colors
dend <- color_branches(dend, k = 8, col = cols_branches)

# Manually match the labels, to the nth column denoting color
labels_colors(dend) <- as.numeric(as.factor(Day9_data_labels[,23])[order.dendrogram(dend)])
labels_colors(dend) <- as.numeric(as.factor(Day9_data_labels[,23])[order.dendrogram(dend)])
labels_colors(dend) <- as.numeric(as.factor(Day9_data_labels[,26])[order.dendrogram(dend)])

# Manually set the second column to leaf label
labels(dend) <- as.character(Day9_data_labels[,3])[order.dendrogram(dend)]
labels(dend) <- as.character(Day9_data_labels[,23])[order.dendrogram(dend)]

labels(dend) <- as.character(Day9_data_labels[,31])[order.dendrogram(dend)]

# Color the branches based on the clusters:
dend %>% color_branches(dend, k=8, groupLabels = TRUE) %>% plot(main = "50k SNP Dendrogram",sub = "8 Genotypic Clusters",horiz = TRUE)

#Circlize dendrogram
circlize_dendrogram(dend, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)

# output dendrogram as TIFF File
tiff("C:/Users/falk/Google Drive/PhD/YouTube Tutorials/test_dendrogram1.tiff", width = 7, height = 7, units = 'in', res = 300)
par(mar = rep(0,4))
circlize_dendrogram(dend, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)
dev.off()

remove(dend)
