library(reshape2)
library(ggplot2)
library(RCurl)
library(dplyr)

#create function to identify maximum value in a column
colMax <- function(data) sapply(data, max, na.rm = TRUE)
#create function to identify minimum value in a column
colMin <- function(data) sapply(data, min, na.rm = TRUE)
#create function to identify mean value in a column
colmean <- function(data) sapply(data, mean, na.rm = TRUE)

#Import new df
AllData <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall_TRL_GR.csv", header = T)
metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv",header = T)
#split data into df Days
Day6_data <- subset(AllData, AllData$Day == "6")
Day9_data <- subset(AllData, AllData$Day == "9")
Day12_data <- subset(AllData, AllData$Day == "12")
#Select then filter
Day6_metadata <- subset(metadata, metadata$Day == "6")
Day9_metadata <- subset(metadata, metadata$Day == "9")
Day12_metadata <- subset(metadata, metadata$Day == "12")

#set all clusters as factor
cols = c(3:ncol(Day9_metadata))
Day9_metadata[cols] <- lapply(Day9_metadata[cols], factor)
colnames(Day9_metadata)
#############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################
Pheno.8 <- Day9_metadata$Phenotraits13_complete
Day9_numbers <- Day9_data[,c(32:72,80)]
Day9_numbers_scaled <- scale(as.matrix(Day9_numbers))
Day9_scaled <- as.data.frame(cbind(Entry,Pheno.8,Day9_numbers_scaled))
Day9_traitz <- as.data.frame(cbind(Entry,Pheno.8,Day9_numbers))
TRL <- Day9_traitz$TRL

Pheno.8_Means <- Day9_scaled %>% dplyr::select(Pheno.8,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRL_GR,TRLUpper,Root_weight) %>% group_by(Pheno.8) %>% dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),SOL=mean(SOL),LED=mean(LED),RHZO=mean(RHZO),TRL_GR=mean(TRL_GR),TRLUpper=mean(TRLUpper),Root_weight=mean(Root_weight))
write.csv(Pheno.8_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/Pheno.8_means.csv")

#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
day9 <-  Day9_scaled %>%
            dplyr::select(Pheno.8,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRL_GR,TRLUpper,Root_weight)
Day9_melt <-  melt(day9, id.vars = "Pheno.8")
Day9_melt$Pheno.8 <- as.factor(Day9_melt$Pheno.8)
#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################

Pheno.8_melt <-  melt(Pheno.8_Means, id.vars = "Pheno.8")

Pheno.8_melt$Pheno.8 <- as.factor(Pheno.8_melt$Pheno.8)


tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_PBC_Day9_April25_SOL_COLOR.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value,color=Day9_melt$Pheno.8)) +
  scale_fill_manual(values=c("#FF0000FF","orange2","yellow2","green3","#00FFFFFF","#0040FFFF","#8000FFFF","#FF00BFFF"))+
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Pheno.8_melt, aes(x=Pheno.8_melt$variable,y=Pheno.8_melt$value, group=Pheno.8, color=Pheno.8), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("#FF0000FF","orange2","yellow2","green3","#00FFFFFF","#0040FFFF","#8000FFFF","#FF00BFFF"))+
  labs(color=expression(atop('Phenotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()


tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_PBC_Day9_April25_SOL_No_Color.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value)) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Pheno.8_melt, aes(x=Pheno.8_melt$variable,y=Pheno.8_melt$value, group=Pheno.8, color=Pheno.8), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("#FF0000FF","orange2","yellow2","green3","#00FFFFFF","#0040FFFF","#8000FFFF","#FF00BFFF"))+
  labs(color=expression(atop('Phenotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
#############################################################################################################
##################################
rainbow(8)
("#FF0000FF","#FFBF00FF","#80FF00FF","#00FF40FF","#00FFFFFF","#0040FFFF","#8000FFFF","#FF00BFFF")