library(colorpanel)
library(dendextend)
library(circlize)
library(colorspace)
library(colorRamps)
library(RColorBrewer)
library(dichromat)
library(gplots)
install.packages("viridis")
library(viridis)

snp35k_dend <- as.dendrogram(hclust(gdist1,method='ward.D'))
cols_branches <-c("yellow2","magenta","red","orange","blue","black","cyan","green3")
snp35k_dend <- set(snp35k_dend, "branches_lwd", 2)
snp35k_dend <- color_branches(snp35k_dend, k=8, col = cols_branches)
plot(snp35k_dend)

metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")

Day6_metadata <- metadata %>% filter(Day == 6)
Day9_metadata <- metadata %>% filter(Day == 9)
Day12_metadata <- metadata %>% filter(Day == 12)

Day9_13traits <- Day9_data %>% dplyr::select(TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight)
Day9_ideotypes <- Day9_metadata %>% dplyr::select(BestGenotype,Scavenger,DT,Beard,Umbrella)
Day9_ideotypes <- lapply(Day9_ideotypes, as.numeric)%>% as.data.frame()
Day9_ideotypes_scaled <- scale(as.matrix(Day9_ideotypes))
Day9_ideotypes_hc <- hclust(as.dist(1-cor(Day9_ideotypes_scaled, method="spearman")), method="complete")
Day9_13traits_scaled <- scale(as.matrix(Day9_13traits))
Day9_traits13_hc <- hclust(as.dist(1-cor(Day9_13traits_scaled, method="spearman")), method="complete")
Day9_scaled <- cbind(Day9_ideotypes_scaled,Day9_13traits_scaled)
hr <- hclust(as.dist(1-cor(t(Day9_scaled), method="pearson")), method="complete")
hc <- hclust(as.dist(1-cor(Day9_scaled, method="spearman")), method="complete")
mycl <- cutree(hr, k = 8) 
myrw <- cutree(hc, k = 8)

mycolhc <- mycolhc[(Day9_metadata$Diversity)] 
mycolhc_order_Day9 <- mycolhc
mycol_blueorange <- scale_fill_viridis()
mycol_blueorange <- colorpanel(292,"orange","white", "darkblue")
mycol_blackred <- colorpanel(292, "black", "white", "red")


tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day9_heatmap_iRoot_March19.tiff",compression = "lzw", width = 5.5, height = 15, units = 'in', res = 500)
heatmap.2(x=Day9_ideotypes_scaled, Rowv=snp35k_dend, Colv=as.dendrogram(Day9_ideotypes_hc), col=mycol_blackred, scale="none", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day9_data$Entry,
          keysize = 1, main = "Day 9 iRoot types Normalized Score")
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day9_heatmap_13Traits_March19.tiff",compression = "lzw", width = 13, height = 15, units = 'in', res = 500)
heatmap.2(x=Day9_13traits_scaled, Rowv=snp35k_dend, Colv=as.dendrogram(Day9_traits13_hc), col=mycol_blueorange, scale="none", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day9_data$Entry,
          keysize = 1, main = "Day 9 iRoot types Normalized Score")
dev.off()

Day9_ideotypes_scaled %>% as.data.frame() %>% dplyr::summarize(Mean = mean(Beard))
Day9_ideotypes_scaled %>% as.data.frame() %>% dplyr::summarize(Mean = mean(Umbrella))

write.csv(Day9_ideotypes_scaled,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day9_ideotypes_scaled.csv")

Day9_beard_scaled <- scale(as.matrix(Day9_ideotypes$Beard))
Day9_umbrella_scaled <- scale(as.matrix(Day9_ideotypes$Umbrella))
Day9_DT_scaled <- scale(as.matrix(Day9_ideotypes$DT))
min(Day9_DT_scaled)
max(Day9_DT_scaled)
min(Day9_beard_scaled)
max(Day9_beard_scaled)

plot(Day9_ideotypes_scaled)
plot(c(1:292),Day9_ideotypes_scaled)
plot(c(1:292),Day9_DT_scaled)

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day9_heatmap_13Traits_March19.tiff",compression = "lzw", width = 30, height = 15, units = 'in', res = 400)
heatmap.2(x=Day9_13traits_scaled, Rowv=snp35k_dend, Colv=as.dendrogram(Day9_traits13_hc), col=mycol_blueorange, scale="none", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day9_data$Entry,
          keysize = 1, main = "Day 9 iRoot types Normalized Score")
dev.off()

#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
Day12_13traits <- Day12_data %>% dplyr::select(TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight)
Day12_ideotypes <- Day12_metadata %>% dplyr::select(BestGenotype,Scavenger,DT,Beard,Umbrella)
Day12_ideotypes <- lapply(Day12_ideotypes, as.numeric)%>% as.data.frame()
Day12_ideotypes_scaled <- scale(as.matrix(Day12_ideotypes))
Day12_ideotypes_hc <- hclust(as.dist(1-cor(Day12_ideotypes_scaled, method="spearman")), method="complete")
Day12_13traits_scaled <- scale(as.matrix(Day12_13traits))
Day12_traits13_hc <- hclust(as.dist(1-cor(Day12_13traits_scaled, method="spearman")), method="complete")
Day12_scaled <- cbind(Day12_ideotypes_scaled,Day12_13traits_scaled)
hr <- hclust(as.dist(1-cor(t(Day12_scaled), method="pearson")), method="complete")
hc <- hclust(as.dist(1-cor(Day12_scaled, method="spearman")), method="complete")
mycl <- cutree(hr, k = 8) 
myrw <- cutree(hc, k = 8)

mycolhc <- mycolhc[(Day12_metadata$Diversity)] 
mycolhc_order_Day12 <- mycolhc
mycol_blueorange <- scale_fill_viridis()
mycol_blueorange <- colorpanel(292,"orange","white", "darkblue")
mycol_blackred <- colorpanel(292, "black", "white", "red")


tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day12_heatmap_iRoot_March19.tiff",compression = "lzw", width = 5.5, height = 15, units = 'in', res = 500)
heatmap.2(x=Day12_ideotypes_scaled, Rowv=snp35k_dend, Colv=as.dendrogram(Day12_ideotypes_hc), col=mycol_blackred, scale="none", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day12_data$Entry,
          keysize = 1, main = "Day 12 iRoot types Normalized Score")
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day12_heatmap_13Traits_March19.tiff",compression = "lzw", width = 13, height = 15, units = 'in', res = 500)
heatmap.2(x=Day12_13traits_scaled, Rowv=snp35k_dend, Colv=as.dendrogram(Day12_traits13_hc), col=mycol_blueorange, scale="none", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day12_data$Entry,
          keysize = 1, main = "Day 12 iRoot types Normalized Score")
dev.off()
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
Day6_13traits <- Day6_data %>% dplyr::select(TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRLUpper,Root_weight)
Day6_ideotypes <- Day6_metadata %>% dplyr::select(BestGenotype,Scavenger,DT,Beard,Umbrella)
Day6_ideotypes <- lapply(Day6_ideotypes, as.numeric)%>% as.data.frame()
Day6_ideotypes_scaled <- scale(as.matrix(Day6_ideotypes))
Day6_ideotypes_hc <- hclust(as.dist(1-cor(Day6_ideotypes_scaled, method="spearman")), method="complete")
Day6_13traits_scaled <- scale(as.matrix(Day6_13traits))
Day6_traits13_hc <- hclust(as.dist(1-cor(Day6_13traits_scaled, method="spearman")), method="complete")
Day6_scaled <- cbind(Day6_ideotypes_scaled,Day6_13traits_scaled)
hr <- hclust(as.dist(1-cor(t(Day6_scaled), method="pearson")), method="complete")
hc <- hclust(as.dist(1-cor(Day6_scaled, method="spearman")), method="complete")
mycl <- cutree(hr, k = 8) 
myrw <- cutree(hc, k = 8)

mycolhc <- mycolhc[(Day6_metadata$Diversity)] 
mycolhc_order_Day6 <- mycolhc
mycol_blueorange <- scale_fill_viridis()
mycol_blueorange <- colorpanel(292,"orange","white", "darkblue")
mycol_blackred <- colorpanel(292, "black", "white", "red")


tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day6_heatmap_iRoot_March19.tiff",compression = "lzw", width = 5.5, height = 15, units = 'in', res = 500)
heatmap.2(x=Day6_ideotypes_scaled, Rowv=snp35k_dend, Colv=as.dendrogram(Day6_ideotypes_hc), col=mycol_blackred, scale="none", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day6_data$Entry,
          keysize = 1, main = "Day 6 iRoot types Normalized Score")
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Heatmap/Day6_heatmap_13Traits_March19.tiff",compression = "lzw", width = 13, height = 15, units = 'in', res = 500)
heatmap.2(x=Day6_13traits_scaled, Rowv=snp35k_dend, Colv=as.dendrogram(Day6_traits13_hc), col=mycol_blueorange, scale="none", density.info="none", 
          trace="none",margins=c(15,2), cexRow = 0.3, cexCol = 1.6, labRow = Day6_data$Entry,
          keysize = 1, main = "Day 6 iRoot types Normalized Score")
dev.off()