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
AllData <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/BlueSteelData_Sept25_thinned.csv", header = T)
metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv",header = T)
#split data into df Days
AllData <- AllData %>%  filter(((Entry == "127")|(Entry == "298")|(Entry  == "199")))
metadata <- metadata %>%  filter(((Entry == "127")|(Entry == "298")|(Entry  == "199")))

Day6_data <- subset(AllData, AllData$Day == "6")
Day9_data <- subset(AllData, AllData$Day == "9")
Day12_data <- subset(AllData, AllData$Day == "12")
#Select then filter
Day6_metadata <- subset(metadata, metadata$Day == "6")
Day9_metadata <- subset(metadata, metadata$Day == "9")
Day12_metadata <- subset(metadata, metadata$Day == "12")

#set all clusters as factor
cols = c(3:ncol(Day6_metadata))
Day6_metadata[cols] <- lapply(Day6_metadata[cols], factor)
colnames(Day6_metadata)
colnames(Day6_data)
#############################################################################################################
######################### Day 6 Extracting MEANS from all data ##############################################
#############################################################################################################
Entry.3 <- Day6_data$Entry
Day6_numbers <- Day6_data[,c(20:61)]
Day6_numbers_scaled <- scale(as.matrix(Day6_numbers))
Day6_scaled <- as.data.frame(cbind(Entry.3,Day6_numbers_scaled))
Day6_traitz <- as.data.frame(cbind(Entry.3,Day6_numbers))
TRL <- Day6_traitz$TRL

Entry.3_Means <- Day6_scaled %>% dplyr::select(Entry.3,TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight) %>% group_by(Entry.3) %>% 
  dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),LED=mean(LED),RHZO=mean(RHZO),WDR=mean(WDR),Root_weight=mean(Root_weight),Shoot_weight=mean(Shoot_weight))
#write.csv(Entry.3_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/Entry.3_means.csv")

#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
Day6 <-  Day6_scaled %>%
  dplyr::select(Entry.3,TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight)
Day6_melt <-  melt(Day6, id.vars = "Entry.3")
Day6_melt$Entry.3 <- as.factor(Day6_melt$Entry.3)
#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################

Entry.3_melt <-  melt(Entry.3_Means, id.vars = "Entry.3")

Entry.3_melt$Entry.3 <- as.factor(Entry.3_melt$Entry.3)

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/HorseRace_3Geno_Day6_COLOR.tiff", units = "in" ,height = 5, width = 8, res=200)
ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value,color=Day6_melt$Entry.3)) +
  scale_fill_manual(values=c("blue","red","green3"))+
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Entry.3_melt, aes(x=Entry.3_melt$variable,y=Entry.3_melt$value, group=Entry.3, color=Entry.3), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","green3","red"))+
  labs(color=('Genotype')) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
#############################################################################################################

#set all clusters as factor
cols = c(3:ncol(Day9_metadata))
Day9_metadata[cols] <- lapply(Day9_metadata[cols], factor)
colnames(Day9_metadata)
colnames(Day9_data)
#############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################
Entry.3 <- Day9_data$Entry
Day9_numbers <- Day9_data[,c(20:61)]
Day9_numbers_scaled <- scale(as.matrix(Day9_numbers))
Day9_scaled <- as.data.frame(cbind(Entry.3,Day9_numbers_scaled))
Day9_traitz <- as.data.frame(cbind(Entry.3,Day9_numbers))
TRL <- Day9_traitz$TRL

Entry.3_Means <- Day9_scaled %>% dplyr::select(Entry.3,TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight) %>% group_by(Entry.3) %>% 
  dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),LED=mean(LED),RHZO=mean(RHZO),WDR=mean(WDR),Root_weight=mean(Root_weight),Shoot_weight=mean(Shoot_weight))
#write.csv(Entry.3_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/Entry.3_means.csv")

#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
day9 <-  Day9_scaled %>%
                  dplyr::select(Entry.3,TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight)
Day9_melt <-  melt(day9, id.vars = "Entry.3")
Day9_melt$Entry.3 <- as.factor(Day9_melt$Entry.3)
#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################

Entry.3_melt <-  melt(Entry.3_Means, id.vars = "Entry.3")

Entry.3_melt$Entry.3 <- as.factor(Entry.3_melt$Entry.3)

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/HorseRace_3Geno_Day9_COLOR.tiff", units = "in" ,height = 5, width = 8, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value,color=Day9_melt$Entry.3)) +
  scale_fill_manual(values=c("blue","red","green3"))+
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Entry.3_melt, aes(x=Entry.3_melt$variable,y=Entry.3_melt$value, group=Entry.3, color=Entry.3), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","green3","red"))+
  labs(color=('Genotype')) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
#############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################
Entry.3 <- Day6_data$Entry
Day6_numbers <- Day6_data[,c(20:61)]
Day6_numbers_scaled <- scale(as.matrix(Day6_numbers))
Day6_scaled <- as.data.frame(cbind(Entry.3,Day6_numbers_scaled))
Day6_traitz <- as.data.frame(cbind(Entry.3,Day6_numbers))
TRL <- Day6_traitz$TRL

Entry.3_Means <- Day6_scaled %>% dplyr::select(Entry.3,TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight) %>% group_by(Entry.3) %>% 
  dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),LED=mean(LED),RHZO=mean(RHZO),WDR=mean(WDR),Root_weight=mean(Root_weight),Shoot_weight=mean(Shoot_weight))
#write.csv(Entry.3_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/Entry.3_means.csv")

#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
Day6 <-  Day6_scaled %>%
  dplyr::select(Entry.3,TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight)
Day6_melt <-  melt(Day6, id.vars = "Entry.3")
Day6_melt$Entry.3 <- as.factor(Day6_melt$Entry.3)
#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################

Entry.3_melt <-  melt(Entry.3_Means, id.vars = "Entry.3")

Entry.3_melt$Entry.3 <- as.factor(Entry.3_melt$Entry.3)

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/HorseRace_3Geno_Day6_COLOR.tiff", units = "in" ,height = 5, width = 8, res=200)
ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value,color=Day6_melt$Entry.3)) +
  scale_fill_manual(values=c("blue","red","green3"))+
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Entry.3_melt, aes(x=Entry.3_melt$variable,y=Entry.3_melt$value, group=Entry.3, color=Entry.3), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","green3","red"))+
  labs(color=('Genotype')) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
#############################################################################################################
######################### Day 12 Extracting MEANS from all data ##############################################
#############################################################################################################
Entry.3 <- Day12_data$Entry
Day12_numbers <- Day12_data[,c(20:61)]
Day12_numbers_scaled <- scale(as.matrix(Day12_numbers))
Day12_scaled <- as.data.frame(cbind(Entry.3,Day12_numbers_scaled))
Day12_traitz <- as.data.frame(cbind(Entry.3,Day12_numbers))
TRL <- Day12_traitz$TRL

Entry.3_Means <- Day12_scaled %>% dplyr::select(Entry.3,TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight) %>% group_by(Entry.3) %>% 
  dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),LED=mean(LED),RHZO=mean(RHZO),WDR=mean(WDR),Root_weight=mean(Root_weight),Shoot_weight=mean(Shoot_weight))
#write.csv(Entry.3_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/Entry.3_means.csv")

#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
Day12 <-  Day12_scaled %>%
  dplyr::select(Entry.3,TRL,PRL,WID,CVA,LRB,VOL,LRA,LED,RHZO,WDR,Root_weight,Shoot_weight)
Day12_melt <-  melt(Day12, id.vars = "Entry.3")
Day12_melt$Entry.3 <- as.factor(Day12_melt$Entry.3)
#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################

Entry.3_melt <-  melt(Entry.3_Means, id.vars = "Entry.3")

Entry.3_melt$Entry.3 <- as.factor(Entry.3_melt$Entry.3)

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/HorseRace_3Geno_Day12_COLOR.tiff", units = "in" ,height = 5, width = 8, res=200)
ggplot(data=Day12_melt, aes(x=Day12_melt$variable, y=Day12_melt$value,color=Day12_melt$Entry.3)) +
  scale_fill_manual(values=c("blue","red","green3"))+
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 12d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Entry.3_melt, aes(x=Entry.3_melt$variable,y=Entry.3_melt$value, group=Entry.3, color=Entry.3), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","green3","red"))+
  labs(color=('Genotype')) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()