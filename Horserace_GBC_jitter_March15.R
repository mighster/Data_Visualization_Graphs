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
colnames(Day9_data)
#############################################################################################################
######################### Day 6 Extracting MEANS from all data ##############################################
#############################################################################################################
snp35k <- Day6_data$snp35k
Entry <- Day6_data$Entry
colnames(Day6_data)
Day6_numbers <- Day6_data[,c(32:72)]
Day6_numbers_scaled <- scale(as.matrix(Day6_numbers))
Day6_scaled <- as.data.frame(cbind(Entry,snp35k,Day6_numbers_scaled))
Day6_traitz <- as.data.frame(cbind(Entry,snp35k,Day6_numbers))
TRL <- Day6_traitz$TRL

snp35k_Means <- Day6_scaled %>% dplyr::select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRLUpper,Root_weight) %>% group_by(snp35k) %>% dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),SOL=mean(SOL),LED=mean(LED),RHZO=mean(RHZO),TRLUpper=mean(TRLUpper),Root_weight=mean(Root_weight))
write.csv(snp35k_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/snp35k_means.csv")

Day6 <-  Day6_scaled %>%
  dplyr::select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRLUpper,Root_weight)
Day6_melt <-  melt(Day6, id.vars = "snp35k")
Day6_melt$snp35k <- as.factor(Day6_melt$snp35k)

snp35k_melt <-  melt(snp35k_Means, id.vars = "snp35k")

snp35k_melt$snp35k <- as.factor(snp35k_melt$snp35k)

  #############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################
snp35k <- Day9_data$snp35k

Day9_numbers <- Day9_data[,c(32:72,80)]
Day9_numbers_scaled <- scale(as.matrix(Day9_numbers))
Day9_scaled <- as.data.frame(cbind(Entry,snp35k,Day9_numbers_scaled))
Day9_traitz <- as.data.frame(cbind(Entry,snp35k,Day9_numbers))
TRL <- Day9_traitz$TRL

snp35k_Means <- Day9_scaled %>% dplyr::select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRL_GR,TRLUpper,Root_weight) %>% group_by(snp35k) %>% dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),SOL=mean(SOL),LED=mean(LED),RHZO=mean(RHZO),TRL_GR=mean(TRL_GR),TRLUpper=mean(TRLUpper),Root_weight=mean(Root_weight))
write.csv(snp35k_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/snp35k_means.csv")



#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
day9 <-  Day9_scaled %>%
            dplyr::select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRL_GR,TRLUpper,Root_weight)
Day9_melt <-  melt(day9, id.vars = "snp35k")
Day9_melt$snp35k <- as.factor(Day9_melt$snp35k)
#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################

snp35k_melt <-  melt(snp35k_Means, id.vars = "snp35k")

snp35k_melt$snp35k <- as.factor(snp35k_melt$snp35k)

#############################################################################################################
######################### Day 12 Extracting MEANS from all data ##############################################
#############################################################################################################
snp35k <- Day12_data$snp35k
Entry <- Day12_data$Entry
colnames(Day12_data)
Day12_numbers <- Day12_data[,c(32:72,80)]
Day12_numbers_scaled <- scale(as.matrix(Day12_numbers))
Day12_scaled <- as.data.frame(cbind(Entry,snp35k,Day12_numbers_scaled))
Day12_traitz <- as.data.frame(cbind(Entry,snp35k,Day12_numbers))
TRL <- Day12_traitz$TRL

snp35k_Means <- Day12_scaled %>% dplyr::select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRL_GR,TRLUpper,Root_weight) %>% group_by(snp35k) %>% dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),SOL=mean(SOL),LED=mean(LED),RHZO=mean(RHZO),TRL_GR=mean(TRL_GR),TRLUpper=mean(TRLUpper),Root_weight=mean(Root_weight))
write.csv(snp35k_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/snp35k_means.csv")

Day12 <-  Day12_scaled %>%
  dplyr::select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRLUpper,TRL_GR,Root_weight)
Day12_melt <-  melt(Day12, id.vars = "snp35k")
Day12_melt$snp35k <- as.factor(Day12_melt$snp35k)

snp35k_melt <-  melt(snp35k_Means, id.vars = "snp35k")

snp35k_melt$snp35k <- as.factor(snp35k_melt$snp35k)


tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_GBC_Day9_April25_SOL_COLOR.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value,color=Day9_melt$snp35k)) +
  scale_fill_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=snp35k_melt, aes(x=snp35k_melt$variable,y=snp35k_melt$value, group=snp35k, color=snp35k), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()


tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_GBC_Day9_April25_SOL_No_Color.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value)) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=snp35k_melt, aes(x=snp35k_melt$variable,y=snp35k_melt$value, group=snp35k, color=snp35k), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
#############################################################################################################
##################################
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_GBC_Day6_March15_COLOR.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value,color=Day6_melt$snp35k)) +
  scale_fill_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=snp35k_melt, aes(x=snp35k_melt$variable,y=snp35k_melt$value, group=snp35k, color=snp35k), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_GBC_Day6_March15_No_Color.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value)) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=snp35k_melt, aes(x=snp35k_melt$variable,y=snp35k_melt$value, group=snp35k, color=snp35k), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
#############################################################################################################
##################################
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_GBC_Day12_March15_COLOR.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day12_melt, aes(x=Day12_melt$variable, y=Day12_melt$value,color=Day12_melt$snp35k)) +
  scale_fill_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 12d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=snp35k_melt, aes(x=snp35k_melt$variable,y=snp35k_melt$value, group=snp35k, color=snp35k), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_GBC_Day12_March15_No_Color.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day12_melt, aes(x=Day12_melt$variable, y=Day12_melt$value)) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 12d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=snp35k_melt, aes(x=snp35k_melt$variable,y=snp35k_melt$value, group=snp35k, color=snp35k), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()

(values=c("magenta","orange","yellow2","green3","cyan","red","black","blue")) +