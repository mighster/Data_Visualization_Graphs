library(colorpanel)
library(dendextend)
library(circlize)
library(colorspace)
library(colorRamps)
library(RColorBrewer)
library(gplots)

snp35k_dend <- as.dendrogram(hclust(gdist1,method='ward.D'))
cols_branches <- c("red","black","orange","yellow2","magenta","cyan","blue","green3")
snp35k_dend <- set(snp35k_dend, "branches_lwd", 2)
snp35k_dend <- color_branches(snp35k_dend, k=8, col = cols_branches)

metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")

Day6_metadata <- metadata %>% filter(Day == 6)
Day9_metadata <- metadata %>% filter(Day == 9)
Day12_metadata <- metadata %>% filter(Day == 12)

Day9_13traits <- Day9_data %>%  
  dplyr::select(TRL,PRL,WID,CVA,LRB,VOL,LRAMediangle,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight)

Day9_ideotypes <- Day9_metadata %>% dplyr::select(BestGenotype,Scavenger,DT,Beard,Umbrella)
Day9_ideotypes <- lapply(Day9_ideotypes, as.numeric)%>% as.data.frame()

#Day9_ideotypes <- Day9_ideotypes %>% mutate(BestGenotypeINV = 293 - BestGenotype,
                                              ScavengerINV = 293 - Scavenger,
                                              DTINV = 293 - DT,
                                              BeardINV = 293 - Beard,
                                              UmbrellaINV = 293 - Umbrella)
#Day9_ideotypes_scaled <- scale(as.matrix(Day9_ideotypes[,6:10]))
Day9_ideotypes_scaled <- scale(as.matrix(Day9_ideotypes))

Day9_13traits_scaled <- scale(as.matrix(Day9_13traits))

Day9_scaled <- cbind(Day9_ideotypes_scaled,Day9_13traits_scaled)

hr <- hclust(as.dist(1-cor(t(Day9_scaled), method="pearson")), method="complete")
hc <- hclust(as.dist(1-cor(Day9_scaled, method="spearman")), method="complete")
# define some clusters
mycl <- cutree(hr, k = 8) 
myrw <- cutree(hc, k = 8)

write.csv(mycl,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day9_13traits_clusters_Jan12.csv")

# get a color palette equal to the number of clusters
mycolhc <- cm.colors(292)
#mycolhr <- rainbow(length(unique(myrw)), start=0.1, end=0.9)

mycolhc_order_Day9 <- mycolhc
#mycolhr_order_Day9 <- mycolhr

# create vector of colors for side bar
mycolhc <- mycolhc[(Day9_metadata$Diversity)] 
#mycolhr <- mycolhr[as.vector(myrw)]

## Plot heatmap 
mycol <- colorpanel(75, "darkblue", "white", "orange") # or try redgreen(75)

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day9_heatmap_13Traits_n8_Jan15.png",compression = "lzw", width = 15, height = 15, units = 'in', res = 500)
heatmap.2(x=Day9_scaled, Rowv=snp35k_dend, col=mycol, scale="row", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day9_data$Entry,
          Colv=FALSE, keysize = 1, main = "Day 9 iRoot types Normalized Score")
dev.off()

############################################################################################
##################################### Day 12  ##############################################
############################################################################################


Day12_13traits <- Day12_data %>%  
  dplyr::select(TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight)

Day12_ideotypes <- Day12_metadata %>% dplyr::select(BestGenotype,Scavenger,DT,Beard,Umbrella)
Day12_ideotypes <- lapply(Day12_ideotypes, as.numeric) %>% as.data.frame()
#Day12_ideotypes <- Day12_ideotypes %>% mutate(BestGenotypeINV = 293 - BestGenotype,
                                              ScavengerINV = 293 - Scavenger,
                                              DTINV = 293 - DT,
                                              BeardINV = 293 - Beard,
                                              UmbrellaINV = 293 - Umbrella)
                          

Day12_heater <- cbind(Day12_ideotypes[,1:5],Day12_13traits)
head(Day12_heater)

Day12_scaled <- scale(as.matrix(Day12_heater))
hr <- hclust(as.dist(1-cor(t(Day12_scaled), method="pearson")), method="complete")
hc <- hclust(as.dist(1-cor(Day12_scaled, method="spearman")), method="complete")
# define some clusters
mycl <- cutree(hr, k = 8) 
myrw <- cutree(hc, k = 8)

#write.csv(mycl,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day12_13traits_clusters_Jan12.csv")

# get a color palette equal to the number of clusters
mycolhc <- cm.colors(292)
#mycolhr <- rainbow(length(unique(myrw)), start=0.1, end=0.9)

mycolhc_order_Day12 <- mycolhc
#mycolhr_order_Day12 <- mycolhr

# create vector of colors for side bar
mycolhc <- mycolhc[(Day12_metadata$Diversity)] 
#mycolhr <- mycolhr[as.vector(myrw)]

## Plot heatmap 
mycol <- colorpanel(75, "darkblue", "white", "orange") # or try redgreen(75)

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day12_heatmap_13Traits_n8_Jan15.png",compression = "lzw", width = 15, height = 15, units = 'in', res = 500)
heatmap.2(x=Day12_scaled, Rowv=snp35k_dend, col=mycol, scale="row", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day12_data$Entry,
          Colv=FALSE, keysize = 1, main = "Day 12 iRoot types Normalized Score")
dev.off()
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day12_heatmap_13Traits_n8_Jan15.tiff",compression = "lzw", width = 15, height = 15, units = 'in', res = 500)
heater <- heatmap.2(x=Day12_scaled, Rowv=snp35k_dend, Colv=as.dendrogram(hc), col=mycol, scale="row", density.info="none", trace="none",margins=c(15,2),RowSideColors=mycolhc, cexRow = 0.3, cexCol = 1.6, labRow = Day12_data$Entry)
dev.off()

