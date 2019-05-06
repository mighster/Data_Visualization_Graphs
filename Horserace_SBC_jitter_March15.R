library(reshape2)
library(ggplot2)
library(RCurl)
library(dplyr)
library("RColorBrewer")
display.brewer.all()

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

  #############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################
Shape.8 <- Day9_data$Shape.8
Day9_numbers <- Day9_data[,c(32:72,80)]
Day9_numbers_scaled <- scale(as.matrix(Day9_numbers))
Day9_scaled <- as.data.frame(cbind(Entry,Shape.8,Day9_numbers_scaled))
Day9_traitz <- as.data.frame(cbind(Entry,Shape.8,Day9_numbers))
TRL <- Day9_traitz$TRL

Shape.8_Means <- Day9_scaled %>% dplyr::select(Shape.8,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight) %>% group_by(Shape.8) %>% dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),SOL2=mean(SOL2),LED=mean(LED),RHZO=mean(RHZO),TRL_GR=mean(TRL_GR),TRLUpper=mean(TRLUpper),Root_weight=mean(Root_weight))
write.csv(Shape.8_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/Shape.8_means.csv")

#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
day9 <-  Day9_scaled %>%
            dplyr::select(Shape.8,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight)
Day9_melt <-  melt(day9, id.vars = "Shape.8")

#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################

Shape8_melt <-  melt(Shape.8_Means, id.vars = "Shape.8")

Shape8_melt$Shape.8 <- as.factor(Shape8_melt$Shape.8)
Day9_melt$Shape.8 <- as.factor(Day9_melt$Shape.8)
str(Shape8_melt)

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_SBC_Day9_April25_SOL2.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value)) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Shape8_melt, aes(x=Shape8_melt$variable,y=Shape8_melt$value, group=Shape.8, color=Shape.8), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("#ffff00",'#FF00CC','#999999','#FF6600','#6600FF','#00FFFF','#33CC33',"red")) +
  labs(color=expression(atop('Shape', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
#############################################################################################################
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_SBC_Day9_color_April25_SOL2.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value,color=Day9_melt$Shape.8)) +
  scale_fill_manual(values=c("#ffff00",'#FF00CC','#999999','#FF6600','#6600FF','#00FFFF','#33CC33',"red")) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Shape8_melt, aes(x=Shape8_melt$variable,y=Shape8_melt$value, group=Shape.8, color=Shape.8), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("#ffff00",'#FF00CC','#999999','#FF6600','#6600FF','#00FFFF','#33CC33',"red")) +
  labs(color=expression(atop('Shape', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
##################################