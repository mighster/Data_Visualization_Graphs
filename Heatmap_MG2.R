library(reshape2)
library(ggplot2)
library(gplots)
library(RCurl)
library(dplyr)
library(colorspace)
library(colorRamps)

AllData <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall_TRL_GR.csv", header = T)
metadata <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv", header = T)
df1 <- AllData %>% filter(MG == "2")
colnames(df1)
Day6_data <- subset(df1, df1$Day == "6")
Day9_data <- subset(df1, df1$Day == "9")
Day12_data <- subset(df1, df1$Day == "12")

Day6_num <- Day6_data %>% select(TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight,Seed.Weight)
Day9_num <- Day9_data %>% select(TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,TRL_GR,Root_weight,Shoot_weight,Seed.Weight)
Day12_num <- Day12_data %>% select(TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,TRL_GR,Root_weight,Shoot_weight,Seed.Weight)

#Day6_num <- Day6_data[,c(2:34,43:49)]
str(Day6_num)
Day6_scaled <- scale(as.matrix(Day6_num))
Day9_scaled <- scale(as.matrix(Day9_num))
Day12_scaled <- scale(as.matrix(Day12_num))
Day6_hierarchical_cluster <- (hclust(dist(Day6_scaled),method='complete')) 
Day9_hierarchical_cluster <- (hclust(dist(Day9_scaled),method='complete')) 
Day12_hierarchical_cluster <- (hclust(dist(Day12_scaled),method='complete')) 
dend_Day6 <- (as.dendrogram(Day6_hierarchical_cluster))
dend_Day9 <- (as.dendrogram(Day9_hierarchical_cluster))
dend_Day12 <- (as.dendrogram(Day12_hierarchical_cluster))

hr <- Day6_hierarchical_cluster
#hr <- hclust(as.dist(1-cor(t(Day6_scaled), method="pearson")), method="ward.D")
hc <- hclust(as.dist(1-cor(Day6_scaled, method="spearman")), method="complete") 

Rowv=dend_Day6
Colv=as.dendrogram(hc)
plot(Rowv)
# define some clusters
mycl <- cutree(hr, k = 8) 
myrw <- cutree(hc, k = 5)
# get a color palette equal to the number of clusters

mycolhc <- rainbow(length(unique(mycl)), start=0.1, end=0.9)
mycolhr <- rainbow(length(unique(myrw)), start=0.1, end=0.9)

mycolhc <- c("magenta","cyan","blue","green3","black","red","orange","yellow2")

# create vector of colors for side bar
mycolhc <- mycolhc[as.vector(mycl)] 
mycolhr <- mycolhr[as.vector(myrw)] 

## Plot heatmap 
mycol <- colorpanel(75, "darkblue", "white", "orange2") # or try redgreen(75)

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/Day6_heatmap_MG2.tiff",compression = "lzw", width = 8, height = 8, units = 'in', res = 300)
heatmap.2(x=Day6_scaled, Rowv=as.dendrogram(Day6_hierarchical_cluster), Colv=as.dendrogram(hc), col=mycol, scale="row", density.info="none", trace="none", RowSideColors=mycolhc, ColSideColors = mycolhr) 
dev.off()
#############################################################################################################
hr <- Day9_hierarchical_cluster
#hr <- hclust(as.dist(1-cor(t(Day6_scaled), method="pearson")), method="ward.D")
hc <- hclust(as.dist(1-cor(Day9_scaled, method="spearman")), method="complete") 

Rowv=dend_Day9
Colv=as.dendrogram(hc)
plot(Rowv)
# define some clusters
mycl <- cutree(hr, k = 8) 
myrw <- cutree(hc, k = 5)
# get a color palette equal to the number of clusters

mycolhc <- rainbow(length(unique(mycl)), start=0.1, end=0.9)
mycolhr <- rainbow(length(unique(myrw)), start=0.1, end=0.9)

mycolhc <- c("magenta","cyan","blue","green3","black","red","orange","yellow2")

# create vector of colors for side bar
mycolhc <- mycolhc[as.vector(mycl)] 
mycolhr <- mycolhr[as.vector(myrw)] 

## Plot heatmap 
mycol <- colorpanel(75, "darkblue", "white", "orange2") # or try redgreen(75)

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/Day9_heatmap_MG2.tiff",compression = "lzw", width = 8, height = 8, units = 'in', res = 300)
heatmap.2(x=Day9_scaled, Rowv=as.dendrogram(Day9_hierarchical_cluster), Colv=as.dendrogram(hc), col=mycol, scale="row", density.info="none", trace="none", RowSideColors=mycolhc, ColSideColors = mycolhr) 
dev.off()

#############################################################################################################
hr <- Day12_hierarchical_cluster
#hr <- hclust(as.dist(1-cor(t(Day6_scaled), method="pearson")), method="ward.D")
hc <- hclust(as.dist(1-cor(Day12_scaled, method="spearman")), method="complete") 

Rowv=dend_Day12
Colv=as.dendrogram(hc)
plot(Rowv)
# define some clusters
mycl <- cutree(hr, k = 8) 
myrw <- cutree(hc, k = 5)
# get a color palette equal to the number of clusters

mycolhc <- rainbow(length(unique(mycl)), start=0.1, end=0.9)
mycolhr <- rainbow(length(unique(myrw)), start=0.1, end=0.9)

mycolhc <- c("magenta","cyan","blue","green3","black","red","orange","yellow2")

# create vector of colors for side bar
mycolhc <- mycolhc[as.vector(mycl)] 
mycolhr <- mycolhr[as.vector(myrw)] 

## Plot heatmap 
mycol <- colorpanel(75, "darkblue", "white", "orange2") # or try redgreen(75)

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Submission/Day12_heatmap_MG2.tiff",compression = "lzw", width = 8, height = 8, units = 'in', res = 300)
heatmap.2(x=Day12_scaled, Rowv=as.dendrogram(Day12_hierarchical_cluster), Colv=as.dendrogram(hc), col=mycol, scale="row", density.info="none", trace="none", RowSideColors=mycolhc, ColSideColors = mycolhr) 
dev.off()

