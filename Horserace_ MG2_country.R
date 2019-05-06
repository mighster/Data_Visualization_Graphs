library(reshape2)
library(ggplot2)
library(RCurl)
library(dplyr)

df1 <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/KGF_AdjustedBLUPsAllDays_thinned_Oct24_tall_TRL_GR.csv", header = T)
metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")
colnames(df1)
data <- cbind(df1,metadata[(ncol(metadata))])
MG2data <- data %>% filter(MG == "2")
MG2metadata <- metadata %>% filter(MG == "2")

Day6_data <- subset(MG2data, MG2data$Day == "6")
Day9_data <- subset(MG2data, MG2data$Day == "9")
Day12_data <- subset(MG2data, MG2data$Day == "12")
Day6_MG2metadata <- subset(MG2metadata, MG2metadata$Day == "6")
Day9_MG2metadata <- subset(MG2metadata, MG2metadata$Day == "9")
Day12_MG2metadata <- subset(MG2metadata, MG2metadata$Day == "12")

#############################################################################################################
######################### Day 6 Extracting MEANS from all data ##############################################
#############################################################################################################
#remove row information

Day6_numbers <- Day6_data[32:72]
Day6_numbers_scaled <- scale(as.matrix(Day6_numbers))
Day6NormalizedOutput <- cbind(Day6_MG2metadata$Country,as.data.frame(Day6_numbers_scaled))
colnames(Day6NormalizedOutput)[1] <- "Country"

#############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################
Day9_numbers <- Day9_data[32:72]
Day9_numbers_scaled <- scale(as.matrix(Day9_numbers))
Day9NormalizedOutput <- cbind(Day9_MG2metadata$Country,as.data.frame(Day9_numbers_scaled))
colnames(Day9NormalizedOutput)[1] <- "Country"

#############################################################################################################
######################### Day 12 Extracting MEANS from all data ##############################################
#############################################################################################################
Day12_numbers <- Day12_data[32:72]
Day12_numbers_scaled <- scale(as.matrix(Day12_numbers))
Day12NormalizedOutput <- cbind(Day12_MG2metadata$Country,as.data.frame(Day12_numbers_scaled))
colnames(Day12NormalizedOutput)[1] <- "Country"

#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
#poster.8
colnames(Day12NormalizedOutput)
day6 <-  Day6NormalizedOutput %>%
      dplyr::select(Country,TRL,PRL,WID,CVA,LRB,VOL,DIA,LRA,BSH,SOL2,LED,RHZO,TRLUpper,TRLLower,WDR,SRL.LRB,Root_weight,Shoot_weight)  #dplyr::select RSA traits to use for particular ideotype
Day6_melt <-  melt(day6, id.vars = "Country")

day9 <-  Day9NormalizedOutput %>%
  dplyr::select(Country,TRL,PRL,WID,CVA,LRB,VOL,DIA,LRA,BSH,SOL2,LED,RHZO,TRLUpper,TRLLower,WDR,SRL.LRB,Root_weight,Shoot_weight) #dplyr::select RSA traits to use for particular ideotype
Day9_melt <-  melt(day9, id.vars = "Country")

day12 <-  Day12NormalizedOutput %>%
  dplyr::select(Country,TRL,PRL,WID,CVA,LRB,VOL,DIA,LRA,BSH,SOL2,LED,RHZO,TRLUpper,TRLLower,WDR,SRL.LRB,Root_weight,Shoot_weight) #dplyr::select RSA traits to use for particular ideotypedeotype
Day12_melt <-  melt(day12, id.vars = "Country")

#############################################################################################################
################################## Extract MEANS from Clusters ##############################################
#############################################################################################################

day_list <- list(Day6NormalizedOutput, Day9NormalizedOutput, Day12NormalizedOutput)
DayDataOutput <- data.frame() # Create new data frame to hold output from loop
DayList <- c(6,9,12)

Cluster.List <- c("USA","China","Japan","Korea","Russia","Other")
i="USA"
for (j in c(1:3)){ #this first loop runs through each DAY, one at a time
  Day_data <- day_list[j]
  Day_data <- as.data.frame(Day_data)
  Day <- DayList[j]
  for (i in 1:6){  #this second loop runs through each TRAIT, one at a time
    Cluster <- Cluster.List[i]
    a <- Day_data %>% filter (Day_data$Country == Cluster)
    b <- a[,2:ncol(a)]
    c <- as.data.frame(t(colmean(b)))
    insert <- as.data.frame(c(Day,Cluster,c))
    colnames(insert)[1] <- "Day"
    colnames(insert)[2] <- "Country"
    DayDataOutput <- rbind(DayDataOutput, insert)
  }
}
#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################
NewData <- DayDataOutput[,c(1:ncol(DayDataOutput))]
colnames(NewData)
#set columns as categorical variables
NewData$Day=as.factor(NewData$Day)
NewData$Country=as.factor(NewData$Country)

#dplyr::select only the
selectedTraits <-  NewData %>%
                  dplyr::select(Day,Country,TRL,PRL,WID,CVA,VOL,DIA,LRB,LRA,BSH,SOL2,LED,RHZO,TRLUpper,TRLLower,WDR,SRL.LRB,Root_weight,Shoot_weight) #dplyr::select RSA traits to use for particular ideotype
clust_melt <- melt(selectedTraits, id.vars = c("Day","Country"))

Day6_clust <- subset(clust_melt, clust_melt$Day == "6")
Day6_clust <- Day6_clust[-1]
Day9_clust <- subset(clust_melt, clust_melt$Day == "9")
Day9_clust <- Day9_clust[-1]
Day12_clust <- subset(clust_melt, clust_melt$Day == "12")
Day12_clust <- Day12_clust[-1]

#############################################################################################################
#############################################################################################################

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Figures/Horserace/HorseRace_MG2_Country_Day6_Feb22.tiff", units = "in" ,height = 5, width = 8, res=200)
ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value, color=Country)) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("Maturity Group II RSA Traits at 6d") + 
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Day6_clust, aes(x=Day6_clust$variable,y=Day6_clust$value, group=Country, color=Country), size = 1.65) +
  scale_color_manual(values=c("blue","red","green3","orange","magenta","black"))+
  labs(color=("Country")) +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1,vjust=0.5), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(), legend.text.align = 1)
dev.off()

 tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Figures/Horserace/HorseRace_MG2_Country_Day9_Feb22.tiff", units = "in" ,height = 5, width = 8, res=200)
 ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value, color=Country)) +
   geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
   ylab("Normalized BLUP Value") + xlab("Maturity Group II RSA Traits at 9d") + 
   scale_y_continuous(limits = c(-4,4)) +
   geom_line(data=Day9_clust, aes(x=Day9_clust$variable,y=Day9_clust$value, group=Country, color=Country), size = 1.65) +
   scale_color_manual(values=c("blue","red","green3","orange","magenta","black"))+
   labs(color=("Country")) +
   theme_bw() +
   theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1,vjust=0.5), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(), legend.text.align =1)
 dev.off()
 
 tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Figures/Horserace/HorseRace_MG2_Country_Day12_Feb22.tiff", units = "in" ,height = 5, width = 8, res=200)
 ggplot(data=Day12_melt, aes(x=Day12_melt$variable, y=Day12_melt$value, color=Country)) +
   geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
   ylab("Normalized BLUP Value") + xlab("Maturity Group II RSA Traits at 12d") + 
   scale_y_continuous(limits = c(-4,4)) +
   geom_line(data=Day12_clust, aes(x=Day12_clust$variable,y=Day12_clust$value, group=Country, color=Country), size = 1.65) +
   scale_color_manual(values=c("blue","red","green3","orange","magenta","black"))+
   labs(color=("Country")) +
   theme_bw() +
   theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1,vjust=0.5), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(), legend.text.align =1)
 dev.off()
 