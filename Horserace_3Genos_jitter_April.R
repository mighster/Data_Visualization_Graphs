library(reshape2)
library(ggplot2)
library(RCurl)
library(dplyr)

df1 <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall_TRL_GR.csv", header = T)
metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")
Geno3data <- df1 %>%  filter(((Entry == "127")|(Entry == "298")|(Entry  == "199")))
Geno3metadata <- metadata %>%  filter(((Entry == "127")|(Entry == "298")|(Entry  == "199")))

Day6_data <- subset(Geno3data, Geno3data$Day == "6")
Day9_data <- subset(Geno3data, Geno3data$Day == "9")
Day12_data <- subset(Geno3data, Geno3data$Day == "12")
Day6_Geno3metadata <- subset(Geno3metadata, Geno3metadata$Day == "6")
Day9_Geno3metadata <- subset(Geno3metadata, Geno3metadata$Day == "9")
Day12_Geno3metadata <- subset(Geno3metadata, Geno3metadata$Day == "12")

#############################################################################################################
######################### Day 6 Extracting MEANS from all data ##############################################
#############################################################################################################
#remove row information

Day6_numbers <- Day6_data[32:72]
Day6_numbers_scaled <- scale(as.matrix(Day6_numbers))
Day6NormalizedOutput <- cbind(Day6_Geno3metadata$Stem.Termination,as.data.frame(Day6_numbers_scaled))
colnames(Day6NormalizedOutput)[1] <- "Stem.Termination"

#############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################
Day9_numbers <- Day9_data[32:72]
Day9_numbers_scaled <- scale(as.matrix(Day9_numbers))
Day9NormalizedOutput <- cbind(Day9_Geno3metadata$Stem.Termination,as.data.frame(Day9_numbers_scaled))
colnames(Day9NormalizedOutput)[1] <- "Stem.Termination"

#############################################################################################################
######################### Day 12 Extracting MEANS from all data ##############################################
#############################################################################################################
Day12_numbers <- Day12_data[32:72]
Day12_numbers_scaled <- scale(as.matrix(Day12_numbers))
Day12NormalizedOutput <- cbind(Day12_Geno3metadata$Stem.Termination,as.data.frame(Day12_numbers_scaled))
colnames(Day12NormalizedOutput)[1] <- "Stem.Termination"

#############################################################################################################
######################### Day 6 Extracting MEANS from all data ##############################################
#############################################################################################################
Entry <- Day6_data$Entry
colnames(Day6_data)
Day6_numbers <- Day6_data[,c(32:72)]
Day6_numbers_scaled <- scale(as.matrix(Day6_numbers))
Day6_scaled <- as.data.frame(cbind(Entry,Day6_numbers_scaled))
Day6_traitz <- as.data.frame(cbind(Entry,Day6_numbers))
TRL <- Day6_traitz$TRL

Day6_Means <- Day6_scaled %>% dplyr::select(Entry,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRLUpper,Root_weight) %>% group_by(Entry) %>% dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),VOL=mean(VOL),LRA=mean(LRA),SOL2=mean(SOL2),LED=mean(LED),RHZO=mean(RHZO),TRLUpper=mean(TRLUpper),Root_weight=mean(Root_weight))
write.csv(snp35k_Means,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/snp35k_means.csv")

Day6 <-  Day6_scaled %>%
                    dplyr::select(Entry,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRLUpper,Root_weight)
Day6_melt <-  melt(Day6, id.vars = "Entry")
Day6_melt$Entry <- as.factor(Day6_melt$Entry)

Entry_melt <-  melt(Day6_Means, id.vars = "Entry")

Entry_melt$Entry <- as.factor(Entry_melt$Entry)







#############################################################################################################
##################################
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_GBC_Day6_March15_COLOR.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value,color=Day6_melt$Entry)) +
  scale_fill_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Entry_melt, aes(x=Entry_melt$variable,y=Entry_melt$value, group=Entry, color=Entry), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()