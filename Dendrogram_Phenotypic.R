library(tidyverse) # for data cleaning
library(ggplot2) # for visualization
library(NAM) #nested association mapping - Alencar Xavier, Katy Rainey
library(phylogram)   
library(dendextend)
library(circlize)
library(colorspace)
library(colorRamps)
library(shiny)
library(radarchart)
#Parallel computing
library(snow)
library(doSNOW)
library(parallel)
detectCores()
cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)

#############################################################################################################
########################### https://www.r-bloggers.com/clustering-mixed-data-types-in-r/ ####################
##########https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html#####################
#############################################################################################################

df<-read.csv(".csv")

df <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall_TRL_GR.csv", header = T)
metadata <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")

dend <- as.dendrogram(hclust(gdist1,method='ward.D'))
#Modify so only keep; China, Japan, U.S., and make all other 'other'
metadata$Country <- as.character(metadata$Country)
str(metadata$Country)
metadata$Country[metadata$Country == 'Algeria'] <- 'Other';metadata$Country[metadata$Country == 'France'] <- 'Other'
metadata$Country[metadata$Country == 'Georgia'] <- 'Other';metadata$Country[metadata$Country == 'North Korea'] <- 'Korea'
metadata$Country[metadata$Country == 'Morocco'] <- 'Other';metadata$Country[metadata$Country == 'Poland'] <- 'Other'
metadata$Country[metadata$Country == 'Portugal'] <- 'Other';metadata$Country[metadata$Country == 'Taiwan'] <- 'Asia'
metadata$Country[metadata$Country == 'Turkey'] <- 'Other';metadata$Country[metadata$Country == 'Uzbekistan'] <- 'Other'
metadata$Country[metadata$Country == 'Vietnam'] <- 'Asia';metadata$Country[metadata$Country == 'Yugoslavia'] <- 'Other'
metadata$Country[metadata$Country == 'South Korea'] <- 'Korea'
table(metadata$Country)
Day6_metadata <- subset(metadata, metadata$Day == "6")
Day9_metadata <- subset(metadata, metadata$Day == "9")
Day12_metadata <- subset(metadata, metadata$Day == "12")


#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day12_data <- subset(df, df$Day == "12")

colnames(df)
Day6_dataonly <- Day6_data[,31:ncol(Day6_data)]
Day9_dataonly <- Day9_data[,31:ncol(Day9_data)]
Day12_dataonly <- Day12_data[,31:ncol(Day12_data)]

traits13 <- Day9_data %>% select(WDR, MSL, SRL_LRB, Area, TRLUpper, TRAUpper, MAX, TRL_GR, PER, RHZO, SRL, TRL, NWA)
Day9_traits13_complete <- (hclust(dist(traits13),method='complete')) 
a <- as.factor(cutree(Day9_traits13_complete,k=8))

Day9_traits13_Ward.D <- (hclust(dist(traits13),method='ward.D')) 
Phenotraits13_ward.D <- (cbind(Day9_data$Entry,(cutree(Day9_traits13_Ward.D,k=8))))
write.csv(Phenotraits13_ward.D,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/phenotraits13_wardsD.csv")
b <- as.factor(cutree(Day9_traits13_Ward.D,k=8)) 

xtab <- confusionMatrix(a,b)

confusion1 <- as.table(xtab)
colsum <- colSums(confusion1[1:ncol(confusion1),])
confusion2 <- rbind(confusion1,colsum)
sumrow <- rowSums(confusion2[1:nrow(confusion2),])
confusion3 <- cbind(confusion2,sumrow)
confusion3


hr <- hclust(as.dist(1-cor(t(Day9_13traits), method="pearson")), method="complete")
dend_Day9_traits13 <- (as.dendrogram(Day9_traits13_cluster))
plot(dend_Day9_traits13)
a <- table(Day9_metadata$Country,(cutree(Day9_traits13_cluster,k=8)))
b <- table(Day9_metadata$snp35k,(cutree(Day9_traits13_cluster,k=8)))
c <- table(Day9_metadata$MG,(cutree(Day9_traits13_cluster,k=8)))
d <- table(Day9_metadata$TRL,(cutree(Day9_traits13_cluster,k=8)))
e <- table(Day9_metadata$snp35k,(cutree(Day9_traits13_cluster,k=8)))
f <- cbind(Day9_metadata$Entry,(cutree(Day9_traits13_cluster,k=8)))
cbind(x,Day9_metadata$Phenotraits13)
yyy<-(cutree(Day9_traits13_cluster,k=8))
zzz <- cbind(yyy,Day9_data)
zzz %>% dplyr::group_by(yyy) %>% dplyr::summarise(mean = mean(TRL), mean(PRL))
str(zzz)
zzz$yyy <- as.factor(zzz$yyy)
?summarize
write.csv(a,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/PhenoTraits13_snp35k_country_table_Jan23.csv")
write.csv(b,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/PhenoTraits13_snp35k_country_clusters_Jan23.csv")

#Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.
Day6_hierarchical_cluster <- (hclust(dist(Day6_dataonly),method='ward.D')) 
Day9_hierarchical_cluster <- (hclust(dist(Day9_dataonly),method='ward.D')) 
Day12_hierarchical_cluster <- (hclust(dist(Day12_dataonly),method='ward.D')) 

dend_Day6 <- (as.dendrogram(Day6_hierarchical_cluster))
dend_Day9 <- (as.dendrogram(Day9_hierarchical_cluster))
dend_Day12 <- (as.dendrogram(Day12_hierarchical_cluster))


a <- table(Day6_metadata$Country,(cutree(Day6_hierarchical_cluster,k=6)))
x <- cbind(Day6_metadata$Entry,(cutree(Day6_hierarchical_cluster,k=6)))
b <- table(Day9_metadata$Country,(cutree(Day9_hierarchical_cluster,k=6)))
y <- cbind(Day9_metadata$Entry,(cutree(Day9_hierarchical_cluster,k=6)))
c <- table(Day12_metadata$Country,(cutree(Day12_hierarchical_cluster,k=6)))
z <- cbind(Day12_metadata$Entry,(cutree(Day12_hierarchical_cluster,k=6)))
cluster_table <- cbind(a,b,c)
cluster_k6 <- rbind(x,y,z)
a
write.csv(cluster_table,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Phenotypic_snp35k6_country_table_Dec17.csv")
write.csv(cluster_k6,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Phenotypic_snp35k6_country_clusters_Dec17.csv")

plot(dend_Day6)
dend_Day6 %>% labels
dend_Day6 %>% nleaves
dend_Day6 %>% nnodes
dend_Day6 %>% head

plot(dend_Day9)
plot(dend_Day12)


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



#############################################################################################################
##################################   Compare Cluster Methods ################################################
#############################################################################################################

#    either by specifying the desired number of groups (k) or the cut height(h).
#   ORDER: 1) By the order of the original data. 2) by the order of the labels in the dendrogram.

# Change the size of the labels
dend_Day6 <- set(dend_Day6, "labels_cex", 0.5)

# Change the thickness of the branches
dend_Day6 <- set(dend_Day6, "branches_lwd", 2)

# get some colors
cols_branches <- c("brown","purple","blue","green3","black","red","orange","yellow2")  #BRANCH COLORS
cols_labels <- c("brown","purple","blue","green3","yellow3","red","orange","black") #BRANCH COLORS

#Set the branches to 8 colors
dend_Day6 <- color_labels(dend_Day6, col = cols_labels) # SNP CUT TREE AND COLORS


#Set the branches to 8 colors
dend_Day6 <- color_branches(dend_Day6, k = 8, col = cols_branches)

# Manually set the second column to leaf label
colnames(Day6_data_labels)
labels(dend_Day6) <- as.character(Day6_data_labels[,22])[order.dendrogram(dend_Day6)]

# Manually match the labels, to the nth column denoting color
labels_colors(dend_Day6) <- as.numeric(as.factor(Day6_data_labels[,22])[order.dendrogram(dend_Day6)]) #change labels here!#change labels here!#change labels here!

dend_Day6 %>% 
  raise.dendrogram (1)

# Color the branches based on the clusters:
dend_Day6 %>% raise.dendrogram (500) %>% color_branches(dend_Day6, k=8, groupLabels = TRUE) %>% plot(main = "50k SNP Dendrogram",sub = "8 Genotypic Clusters",horiz = FALSE)

#Circlize dendrogram
circlize_dendrogram(dend_Day6, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)


# output dendrogram as TIFF File
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Dendrogram/Dendrogram_day6_phenotypic_snp35k6_Leafs_Dec17.tiff", width = 7, height = 7, units = 'in', res = 200)
par(mar=c(0, 0, 0, 0))
circlize_dendrogram(dend_Day6, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)
dev.off()

#############################################################################################################
#############################################################################################################
#############################################################################################################
dend <- as.dendrogram(hclust(gdist1,method='ward.D'))

dend <- as.dendrogram(hclust(gdist1,method='ward.D')) %>%
  set("branches_k_color", k=8) %>% set("branches_lwd", 2) %>%
  set("branches_lty", 1) %>% set("branches_col", c("red","black","orange","yellow2","magenta","cyan","blue","green3")) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
  set("labels_col", c("red","black","orange","yellow2","magenta","cyan","blue","green3"))

dend <- set(dend, "labels_col", c("blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","magenta","black","magenta","black","cyan","cyan","red","blue","red","orange","red","magenta","cyan","red","blue","red","magenta","red","blue","red","magenta","orange","red","red","magenta","red","orange","magenta","red","red","blue","magenta","blue","yellow2","black","magenta","red","orange","red","red","black","black","yellow2","black","red","red","red","magenta","magenta","cyan","red","magenta","magenta","cyan","red","orange","magenta","cyan","cyan","cyan","magenta","magenta","blue","blue","magenta","magenta","orange","red","magenta","orange","cyan","orange","magenta","red","magenta","blue","magenta","magenta","red","orange","cyan","blue","cyan","cyan","red","cyan","magenta","red","magenta","red","red","orange","blue","cyan","cyan","black","blue","green3","green3","cyan","green3","red","red","magenta","orange","yellow2","orange","blue","magenta","yellow2","orange","magenta","red","cyan","blue","cyan","blue","cyan","magenta","cyan","yellow2","magenta","red","yellow2","blue","cyan","blue","orange","blue","cyan","red","green3","magenta","orange","cyan","orange","magenta","green3","magenta","orange","orange","magenta","blue","black","cyan","magenta","magenta","green3","red","cyan","red","orange","magenta","magenta","red","magenta","magenta","black","green3","magenta","magenta","cyan","black","green3","magenta","cyan","red","blue","green3","green3","cyan","yellow2","cyan","blue","green3","orange","yellow2","black","blue","magenta","magenta","magenta","cyan","blue","blue","magenta","black","black","blue","orange","cyan","magenta","red","red","cyan","red","cyan","cyan","cyan","orange","red","magenta","green3","cyan","black","red","magenta","green3","blue","magenta","magenta","magenta","blue","cyan","red","red","magenta","green3","magenta","red","cyan","orange","red","black","cyan","blue","orange","magenta","orange","blue","green3","magenta","magenta","blue","blue","orange","green3","blue","blue","magenta","orange","cyan","cyan","green3","cyan","blue","blue","red","red","blue","magenta","magenta","red","yellow2","magenta","cyan","red","magenta","magenta","blue","red","cyan","magenta","red","green3","orange","cyan","orange","magenta","red","magenta","cyan","cyan","magenta","red","black","magenta","orange","black","red","orange","orange","magenta","green3","orange","red","cyan","orange","orange","black","cyan"))
dend <- set(dend, "labels_col", cols_labels[order.dendrogram(dend)])
circlize_dendrogram(dend, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)

dend_order <- order.dendrogram(dend)
dend_order %>% table()

write.csv(dend_order,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Dendrogram/Dendrogram_Order_Snp35k_WardD.csv")
c("red","black","orange","yellow2","magenta","cyan","blue","green3")[order.dendrogram(dend)]
# Change the size of the labels
dend <- set(dend, "labels_cex", 0.7)

# Change the thickness of the branches
dend <- set(dend, "branches_lwd", 2)

#Set the branches to 8 colors
dend <- color_branches(dend, k = 8)

# get some colors
cols_labels <- rainbow(8)
# get some colors
cols_branches <- c("red","black","orange","yellow2","magenta","cyan","blue","green3")
#Set the branches to 8 colors
dend <- color_branches(dend, k = 8, col = cols_branches)
dend <- color_labels(dend,k=8,col = cols_labels)
# Manually set the second column to leaf label
colnames(Day9_data_labels)
labels(dend) <- as.character(Day9_data_labels[,22])[order.dendrogram(dend)]

# Manually match the labels, to the 12th column denoting color
labels_colors(dend) <- as.numeric(as.factor((Day9_metadata$Phenotraits13_complete)[order.dendrogram(dend)]))
labels_colors(dend) <- as.numeric(as.factor((Day9_metadata$Phenotraits13_wardsD)[order.dendrogram(dend)]))
labels_colors(dend) <- as.numeric(as.factor(Day9_data_labels[,22])[order.dendrogram(dend)]) #change labels here!#change labels here!#change labels here!

dend <- assign_values_to_leaves_nodePar(dend=dend, value = c(3,2), nodePar = "cols_labels")
# Color the branches based on the clusters:
dend %>% color_branches(dend, k=8, groupLabels = TRUE) %>% plot(main = "50k SNP Dendrogram",sub = "8 Genotypic Clusters",horiz = TRUE)

#Circlize dendrogram
circlize_dendrogram(dend, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)

# output dendrogram as TIFF File
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Dendrogram/Dendrogram_Country_snp35k_Feb12.png", width = 7, height = 7, units = 'in', res = 300)
par(mar=c(0, 0, 0, 0))
circlize_dendrogram(dend, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)
dev.off()

# Change the size of the labels
dend_Day12 <- set(dend_Day12, "labels_cex", 0.5)

# Change the thickness of the branches
dend_Day12 <- set(dend_Day12, "branches_lwd", 2)

# get some colors
cols_branches <- c("red","black","orange","yellow2","magenta","cyan","blue","green3")

# get some colors
cols_labels <- c("blue","yellow2","green3","red","magenta","cyan","orange")

#Set the branches to 8 colors
dend_Day12 <- color_branches(dend_Day12, k = 8)

col_labels <- c("blue","yellow2","green3","red","magenta","cyan","orange")

dend <- color_labels(dend, k = 7, col = cols_labels)

# Manually set the second column to leaf label
colnames(Day12_data_labels)
labels(dend_Day12) <- as.character(Day12_data_labels[,22])[order.dendrogram(dend_Day12)]

# Manually match the labels, to the 12th column denoting color
labels_colors(dend_Day12) <- as.numeric(as.factor(Day12_data_labels[,22])[order.dendrogram(dend_Day12)]) #change labels here!#change labels here!#change labels here!

# Color the branches based on the clusters:
dend_Day12 %>% color_branches(dend_Day12, k=8, groupLabels = TRUE) %>% plot(main = "50k SNP Dendrogram",sub = "8 Genotypic Clusters",horiz = TRUE)

#Circlize dendrogram
circlize_dendrogram(dend_Day12, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)

# output dendrogram as TIFF File
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Dendrogram/Dendrogram_Day12_phenotypic_snp35k6_Leafs_Dec17.tiff", width = 7, height = 7, units = 'in', res = 200)
par(mar=c(0, 0, 0, 0))
circlize_dendrogram(dend_Day12, main = "Colored branches",dend_track_height = 0.85, groupLabels = TRUE)
dev.off()

Day9_data_labels %>% select(Country) %>% table()





# WRITE data to file with current date. You can add other options as well, like Day6, Day9 or INFO
currentDate <- Sys.Date() 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/", currentDate,".csv",sep="") 
write.csv(tsne_cluster, file=csvFileName)

getwd()
