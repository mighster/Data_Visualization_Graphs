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
cols = c(3:ncol(metadata))
metadata[cols] <- lapply(metadata[cols], factor)

#############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################
colnames(Day6_data)
Day6_numbers <- Day6_data[32:72]
Day6_numbers_scaled <- scale(as.matrix(Day6_numbers))
Day9_numbers <- Day9_data[,c(32:72,80)]
Day9_numbers_scaled <- scale(as.matrix(Day9_numbers))
Day9_scaled <- as.data.frame(cbind(Entry, Day9_numbers_scaled))
colnames(Day9_scaled)

#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
day9 <-  Day9_scaled %>%
            dplyr::select(Entry,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight)
Day9_melt <-  melt(day9, id.vars = "Entry")

#############################################################################################################
################################## Extract MEANS from Clusters ##############################################
#############################################################################################################

`%>%` <- magrittr::`%>%`

BestGenotype_normalized <- Day9_scaled %>%
                                      filter(Entry %in% BestGenotypeTop10$Entry)%>% #Create meta df with Entry, PI and Cluster info
                                                                               rowwise() %>% 
                                                                                            mutate(Ideotype = "Best Overall")
Scavenger_normalized <- Day9_scaled %>%
                                      filter(Entry %in% ScavengerTop10$Entry)%>% #Create meta df with Entry, PI and Cluster info
                                                                                rowwise() %>% 
                                                                                                 mutate(Ideotype = "Foraging")
SteepDeepCheap_normalized <- Day9_scaled %>%
                                      filter(Entry %in% SteepDeepCheapTop10$Entry)%>% #Create meta df with Entry, PI and Cluster info
                                                                                rowwise() %>% 
                                                                                                   mutate(Ideotype = "Drought-Tolerant")
Beard_normalized <- Day9_scaled %>%
                                      filter(Entry %in% BeardTop10$Entry) %>%#Create meta df with Entry, PI and Cluster info
                                                                              rowwise() %>% 
                                                                                                   mutate(Ideotype = "Beard type")
Umbrella_normalized <- Day9_scaled %>%
                                      filter(Entry %in% UmbrellaTop10$Entry)%>% #Create meta df with Entry, PI and Cluster info
                                                                              rowwise() %>% 
                                                                                                  mutate(Ideotype = "Umbrella type")
SuperTraits <- rbind(BestGenotype_normalized,Scavenger_normalized,SteepDeepCheap_normalized,Beard_normalized,Umbrella_normalized)
colnames(SuperTraits)
SuperTraits$Ideotype
SuperTraitMeans <- SuperTraits %>% group_by(Ideotype) %>% summarise_all(funs(mean))

#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################

SuperTraitMeans <-  SuperTraitMeans %>%
                                 select(TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRL_GR,TRLUpper,Root_weight,Ideotype)
SuperTrait_melt <-  melt(SuperTraitMeans, id.vars = "Ideotype")

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Drafts/HorseRace_Ideotypes_Day9_March18_.tiff", units = "in" ,height = 5, width = 10, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value)) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d") + 
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=SuperTrait_melt, aes(x=SuperTrait_melt$variable,y=SuperTrait_melt$value, group=Ideotype, color=Ideotype), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c( "green2", "yellow2","magenta", "blue", "orange2")) +
  labs(color="iRoot Type")+
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
#############################################################################################################
#############################################################################################################

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/HorseRace/HorseRace_poster.8_Day6.tiff", units = "in" ,height = 6, width = 10, res=200)
  ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value)) +
  geom_point(size =2,na.rm=TRUE) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d") + 
  scale_y_continuous(limits = c(0,1)) +
  geom_line(data=Day6_clust, aes(x=Day6_clust$variable,y=Day6_clust$value, group=poster.8, color=poster.8), size = 1.5) +
  #scale_color_manual(values=c("yellow", "deeppink", "darkorchid","green","blue", "red")) +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 45, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/HorseRace/HorseRace_poster.8_Day9.tiff", units = "in" ,height = 6, width = 10, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value)) +
    geom_point(size =2,na.rm=TRUE) +
    ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d") + 
    scale_y_continuous(limits = c(0,1)) +
    geom_line(data=Day9_clust, aes(x=Day9_clust$variable,y=Day9_clust$value, group=snp35k, color=snp35k), size = 1.5) +
    scale_color_manual(values=c("#660099","#64B5D6","#FF00CC","#CCFF33","#39BEB1","orange","#72BB83","red")) +
    theme_bw() +
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 45, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
  dev.off()
  
  tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/HorseRace/HorseRace_poster.8_Day12.tiff", units = "in" ,height = 6, width = 10, res=200)
  ggplot(data=Day12_melt, aes(x=Day12_melt$variable, y=Day12_melt$value)) +
    geom_point(size =2,na.rm=TRUE) +
    ylab("Normalized BLUP Value") + xlab("RSA Trait at 12d") + 
    scale_y_continuous(limits = c(0,1)) +
    geom_line(data=Day12_clust, aes(x=Day12_clust$variable,y=Day12_clust$value, group=poster.8, color=poster.8), size = 1.5) +
    #scale_color_manual(values=c("yellow", "deeppink", "darkorchid","green","blue", "red")) +
    theme_bw() +
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 45, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
  dev.off()
  
  rainbow_hcl(8)
  scale_color_manual(values=c("#E495A5","#D2A277","#ABB065","#72BB83","#39BEB1","#64B5D6","#ACA4E2","#D995CF"))
  scale_color_manual(values=c("#ACA4E2","#64B5D6","#D995CF","#ABB065","#39BEB1","#D2A277","#72BB83","#E495A5"))
  
  
  #E495A5" "#D2A277" "#ABB065" "#72BB83" "#39BEB1" "#64B5D6" "#ACA4E2" "#D995CF"