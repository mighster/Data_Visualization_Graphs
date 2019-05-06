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
colnames(metadata)

#set all clusters as factor
cols = c(3:ncol(metadata))
metadata[cols] <- lapply(metadata[cols], factor)

Day6_metadata <- metadata %>% filter(Day == "6")
Day9_metadata <- metadata %>% filter(Day == "9")
Day12_metadata <- metadata %>% filter(Day == "12")

############################################################################################
################################## Steep, Deep, Cheap#######################################
############################################################################################
SteepDeepCheap <-  AllData %>%
                    select(LRA,PRL,SOL2,TRL_GR) #select RSA traits to use for particular ideotype
df <- cbind(metadata,SteepDeepCheap)

############################################################################################
##################################### Scavenger ############################################
############################################################################################
Scavenging <-  AllData %>%
                    select(TRLUpper,WID,TRL_GR) #select RSA traits to use for particular ideotype
df <- cbind(metadata,Scavenging)

############################################################################################
################################## Best Genotype ###########################################
############################################################################################
BestGenotype <-  AllData %>%
                     select(Root_weight,TRL,PRL,CVA,VOL,WID,LRB,RHZO) #select RSA traits to use for particular ideotype
df <- cbind(metadata,BestGenotype)

colnames(df)
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################

#split data into df Days
Day6_data <- subset(AllData, AllData$Day == "6")
Day9_data <- subset(AllData, AllData$Day == "9")
Day12_data <- subset(AllData, AllData$Day == "12")

#############################################################################################################
######################### Day 6 Extracting MEANS from all data ##############################################
#############################################################################################################
#Create empty data frame for BLUP output
Day6NormalizedOutput <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry"))))
Entry <- Day6_data$Entry
Day6NormalizedOutput$Entry <- Entry
Day6NormalizedOutput$Day <- c(6)

#remove row information
colnames(Day6_data)
Day6_numbers <- Day6_data[31:ncol(Day6_data)]
str(Day6_numbers)

colnum=c(1:ncol(Day6_numbers)) 

for (i in 1:51){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_numbers)[x] #sets the current column header as the trait name
  Day6ColMax <- colMax(Day6_numbers[x])
  Day6ColMin <- colMin(Day6_numbers[x])
  Day6ColSpread <- Day6ColMax-Day6ColMin
  newcol <- (Day6_numbers[x] - Day6ColMin) / Day6ColSpread
  Day6NormalizedOutput <- cbind(Day6NormalizedOutput,newcol)
}
Day6_numbers_scaled <- scale(as.matrix(Day6_numbers))
Day6NormalizedOutput <- cbind(Day6_metadata$snp35k,as.data.frame(Day6_numbers_scaled))
colnames(Day6NormalizedOutput)[1] <- "snp35k"

#############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################

#Create empty data frame for BLUP output
Day9NormalizedOutput <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry"))))
Entry <- Day9_data$Entry
Day9NormalizedOutput$Entry <- Entry
Day9NormalizedOutput$Day <- c(9)

#remove row information
colnames(Day9_data)
Day9_numbers <- Day9_data[31:ncol(Day9_data)]
str(Day9_numbers)

colnum=c(1:ncol(Day9_numbers)) 

for (i in 1:51){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_numbers)[x] #sets the current column header as the trait name
  Day9ColMax <- colMax(Day9_numbers[x])
  Day9ColMin <- colMin(Day9_numbers[x])
  Day9ColSpread <- Day9ColMax-Day9ColMin
  newcol <- (Day9_numbers[x] - Day9ColMin) / Day9ColSpread
  Day9NormalizedOutput <- cbind(Day9NormalizedOutput,newcol)
}
Day9_numbers_scaled <- scale(as.matrix(Day9_numbers))
Day9NormalizedOutput <- cbind(Day9_metadata$snp35k,as.data.frame(Day9_numbers_scaled))
colnames(Day9NormalizedOutput)[1] <- "snp35k"
#############################################################################################################
######################### Day 12 Extracting MEANS from all data ##############################################
#############################################################################################################

#take only the columns with numerical data
colnum=c(28:ncol(Day12_data)) 

#Create empty data frame for BLUP output
Day12NormalizedOutput <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry"))))
Entry <- Day12_data$Entry
Day12NormalizedOutput$Entry <- Entry
Day12NormalizedOutput$Day <- c(12)
poster.8 <- Day12_data$poster.8
Day12NormalizedOutput$poster.8 <- poster.8

#remove row information
colnames(Day12_numbers)
Day12_numbers <- Day12_data[31:ncol(Day12_data)]
str(Day12_numbers)

colnum=c(1:ncol(Day12_numbers)) 

for (i in 1:51){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_numbers)[x] #sets the current column header as the trait name
  Day12ColMax <- colMax(Day12_numbers[x])
  Day12ColMin <- colMin(Day12_numbers[x])
  Day12ColSpread <- Day12ColMax-Day12ColMin
  newcol <- (Day12_numbers[x] - Day12ColMin) / Day12ColSpread
  Day12NormalizedOutput <- cbind(Day12NormalizedOutput,newcol)
}
Day12_numbers_scaled <- scale(as.matrix(Day12_numbers))
Day12NormalizedOutput <- cbind(Day12_metadata$snp35k,as.data.frame(Day12_numbers_scaled))
colnames(Day12NormalizedOutput)[1] <- "snp35k"
#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
#poster.8
colnames(Day12NormalizedOutput)
day6 <-  Day6NormalizedOutput %>%
  dplyr::select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRLUpper,Root_weight)  #select RSA traits to use for particular ideotype
Day6_melt <-  melt(day6, id.vars = "snp35k")

day9 <-  Day9NormalizedOutput %>%
  select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRLUpper,TRL_GR,Root_weight) #select RSA traits to use for particular ideotype
Day9_melt <-  melt(day9, id.vars = "snp35k")

day12 <-  Day12NormalizedOutput %>%
  select(snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRLUpper,TRL_GR,Root_weight) #select RSA traits to use for particular ideotypedeotype
Day12_melt <-  melt(day12, id.vars = "snp35k")

#############################################################################################################
################################## Extract MEANS from Clusters ##############################################
#############################################################################################################

day_list <- list(Day6NormalizedOutput, Day9NormalizedOutput, Day12NormalizedOutput)
column_names <- colnames(Day6_data[,c(2:ncol(Day6_data))])
DayDataOutput <- data.frame() # Create new data frame to hold output from loop
DayList <- c(6,9,12)

Cluster.List <- c(1,2,3,4,5,6,7,8)

for (j in c(1:3)){ #this first loop runs through each DAY, one at a time
  Day_data <- day_list[j]
  Day_data <- as.data.frame(Day_data)
  Day <- DayList[j]
  for (i in 1:8){  #this second loop runs through each TRAIT, one at a time
    Cluster <- Cluster.List[i]
    a <- Day_data %>% filter (Day_data$snp35k == i)
    b <- a[,4:ncol(a)]
    c <- as.data.frame(t(colmean(b)))
    insert <- as.data.frame(c(Day,Cluster,c))
    colnames(insert)[1] <- "Day"
    colnames(insert)[2] <- "snp35k"
    DayDataOutput <- rbind(DayDataOutput, insert)
  }
}
#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################
NewData <- DayDataOutput[,c(1:ncol(DayDataOutput))]
str(NewData)
#set columns as categorical variables
NewData$Day=as.factor(NewData$Day)
NewData$snp35k=as.factor(NewData$snp35k)

#select only the
SelectedTraits <-  NewData %>%
                  select(Day,snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRLUpper,Root_weight) #select RSA traits to use for particular ideotype
clust_melt <- melt(SelectedTraits, id.vars = c("Day","snp35k"))

Day6_SelectedTraits <-  NewData %>%
      dplyr::select(Day,snp35k,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL2,LED,RHZO,TRLUpper,Root_weight) #select RSA traits to use for particular ideotype
Day6_clust_melt <- melt(Day6_SelectedTraits, id.vars = c("Day","snp35k"))

Day6_clust <- subset(Day6_clust_melt, Day6_clust_melt$Day == "6")
Day6_clust <- Day6_clust[-1]
Day9_clust <- subset(clust_melt, clust_melt$Day == "9")
Day9_clust <- Day9_clust[-1]
Day12_clust <- subset(clust_melt, clust_melt$Day == "12")
Day12_clust <- Day12_clust[-1]

#############################################################################################################
#############################################################################################################

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/HorseRace/HorseRace_snp35k_Day6_Feb5.tiff", units = "in" ,height = 5, width = 8, res=200)
ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value)) +
  geom_point(size =1,na.rm=TRUE) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d based on Complete Clustering") + 
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Day6_clust, aes(x=Day6_clust$variable,y=Day6_clust$value, group=snp35k, color=snp35k), size = 1.5) +
  scale_color_manual(values=c("blue","red","orange","green3","black","yellow2","magenta","cyan"))+
  labs(color=expression(atop("Genotypic", paste("Cluster")))) +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(), legend.text.align = 2)
 dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/HorseRace/HorseRace_snp35k_Day9_Feb5.tiff", units = "in" ,height = 5, width = 8, res=200)
ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y=Day9_melt$value)) +
  geom_point(size =2,na.rm=TRUE) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d") + 
  scale_y_continuous(limits = c(0,1)) +
  geom_line(data=Day9_clust, aes(x=Day9_clust$variable,y=Day9_clust$value, group=snp35k, color=snp35k), size = 1.5) +
  scale_color_manual(values=c("blue","red","orange","green3","black","yellow2","magenta","cyan"))+
  labs(color=expression(atop("Genotypic", paste("Cluster")))) +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/HorseRace/HorseRace_snp35k_Day12_Feb5.tiff", units = "in" ,height = 5, width = 8, res=200)
ggplot(data=Day12_melt, aes(x=Day12_melt$variable, y=Day12_melt$value)) +
  geom_point(size =2,na.rm=TRUE) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 12d") + 
  scale_y_continuous(limits = c(0,1)) +
  geom_line(data=Day12_clust, aes(x=Day12_clust$variable,y=Day12_clust$value, group=snp35k, color=snp35k), size = 1.5) +
  scale_color_manual(values=c("blue","red","orange","green3","black","yellow2","magenta","cyan"))+
  labs(color=expression(atop("Genotypic", paste("Cluster")))) +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
dev.off()
  
  #E495A5" "#D2A277" "#ABB065" "#72BB83" "#39BEB1" "#64B5D6" "#ACA4E2" "#D995CF"