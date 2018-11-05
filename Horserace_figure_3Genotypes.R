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
input_df <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/292_AdjustedBLUPsAllDays.csv")

df <- input_df[!(input_df$Entry %in% c(9,98,273,202,182,190,204,167)),]

#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day12_data <- subset(df, df$Day == "12")


#############################################################################################################

#take only the columns with numerical data
colnum=c(3:44) 

#Create empty data frame for BLUP output
Day6NormalizedOutput <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry"))))
Entry <- Day6_data$Entry
Day6NormalizedOutput$Entry <- Entry
Day6NormalizedOutput$Day <- c(6)
Cluster <- Day6_data$Cluster
Day6NormalizedOutput$Cluster <- Cluster

Day6ColMax <- colMax(Day6_data[,colnum])
Day6ColMin <- colMin(Day6_data[,colnum])
Day6ColSpread <- Day6ColMax-Day6ColMin

#remove row information
Day6_numbers <- Day6_data[colnum]

colnum=c(1:42) 

for (i in 1:42){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_numbers)[x] #sets the current column header as the trait name
  Day6ColMax <- colMax(Day6_numbers[x])
  Day6ColMin <- colMin(Day6_numbers[x])
  Day6ColSpread <- Day6ColMax-Day6ColMin
  newcol <- (Day6_numbers[x] - Day6ColMin) / Day6ColSpread
  Day6NormalizedOutput <- cbind(Day6NormalizedOutput,newcol)
}

#Create empty data frame for BLUP output
Day9NormalizedOutput <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry"))))
Entry <- Day9_data$Entry
Day9NormalizedOutput$Entry <- Entry
Day9NormalizedOutput$Day <- c(9)
Cluster <- Day9_data$Cluster
Day9NormalizedOutput$Cluster <- Cluster

#remove row information
Day9_numbers <- Day9_data[6:47]
colnum=c(1:42) 
for (i in 1:42){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_numbers)[x] #sets the current column header as the trait name
  Day9ColMax <- colMax(Day9_numbers[x])
  Day9ColMin <- colMin(Day9_numbers[x])
  Day9ColSpread <- Day9ColMax-Day9ColMin
  newcol <- (Day9_numbers[x] - Day9ColMin) / Day9ColSpread
  Day9NormalizedOutput <- cbind(Day9NormalizedOutput,newcol)
}

#Create empty data frame for BLUP output
Day12NormalizedOutput <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry"))))
Entry <- Day12_data$Entry
Day12NormalizedOutput$Entry <- Entry
Day12NormalizedOutput$Day <- c(12)
Cluster <- Day12_data$Cluster
Day12NormalizedOutput$Cluster <- Cluster

#remove row information
Day12_numbers <- Day12_data[6:47]
for (i in 1:42){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_numbers)[x] #sets the current column header as the trait name
  Day12ColMax <- colMax(Day12_numbers[x])
  Day12ColMin <- colMin(Day12_numbers[x])
  Day12ColSpread <- Day12ColMax-Day12ColMin
  newcol <- (Day12_numbers[x] - Day12ColMin) / Day12ColSpread
  Day12NormalizedOutput <- cbind(Day12NormalizedOutput,newcol)
}

NormalizedOutput <- rbind(Day6NormalizedOutput,Day9NormalizedOutput,Day12NormalizedOutput)
write.csv(NormalizedOutput, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/NormalizedBLUPsAllDays_292.csv")



#Import new df
#data<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Z-score plots/z-Scores_day6_MG2.csv")

#LETS ONLY GRAPH RSA TRAITS THAT ARE BIOLOGICALLY WORTHY
GraphingData <- NormalizedOutput[,c(2:3,5:35)]
GraphingData$Day=as.factor(GraphingData$Day)
GraphingData$Cluster=as.factor(GraphingData$Cluster)
#MELT it!
postmelt <- melt(GraphingData, id.vars = c("Day","Cluster"))


#split data into df Days
Day6_melt <- subset(postmelt, postmelt$Day == "6")
Day9_melt <- subset(postmelt, postmelt$Day == "9")
Day12_melt <- subset(postmelt, postmelt$Day == "12")

#########################################################################################################################
day_list <- list(Day6NormalizedOutput, Day9NormalizedOutput, Day12NormalizedOutput)

DayDataOutput <- data.frame()
DayList <- c(6,9,12)
ClusterList <- c(1,2,3,4,5,6)

for (j in 1:3){
  Day_data <- day_list[j]
  Day_data <- as.data.frame(Day_data)
  Day <- DayList[j]
  
  for (i in 1:6){  #this second loop runs through each TRAIT, one at a time
    Clust <- ClusterList[i]
    a <- Day_data %>% filter (Cluster == i)
    b <- a[,6:44]
    c <- colmean(b)
    insert <- c(Day,Clust, c)
    DayDataOutput <- rbind(DayDataOutput, insert)
  }
}
colnames(DayDataOutput) <- colnames(Day6_data[,c(3,5:44)])
NewData <- DayDataOutput[,c(1:3,5:34)]
#set columns as categorical variables
NewData$Day=as.factor(NewData$Day)
NewData$Cluster=as.factor(NewData$Cluster)

str(NewData)
str(GraphingData)
clust_melt <- melt(NewData, id.vars = c("Day","Cluster"))

Day6_clust <- subset(clust_melt, clust_melt$Day == "6")
Day9_clust <- subset(clust_melt, clust_melt$Day == "9")
Day12_clust <- subset(clust_melt, clust_melt$Day == "12")

tapply(Day6_data$TRL, Day6_data$Cluster, mean)
#############################################################################################################

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/HorseRace_Day6.tiff", units = "in" ,height = 8, width = 21, res=300)
  ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y=Day6_melt$value)) +
  geom_point(size =3,na.rm=TRUE) 
   # ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d") + 
    #scale_y_continuous(limits = c(0,1)) + 
  geom_line(data=Day6_clust, aes(x=Day6_clust$variable,y=Day6_clust$value, group=Cluster, color=Cluster), size = 2)  
    #scale_color_manual(values=c("#6090ff", "#ff4c4c", "#55d655","green","blue", "red")) +
    #theme_bw() +
   # theme(text = element_text(size=15),axis.text.x = element_text(angle = 45, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="none")
#dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/HorseRace_Day9.tiff", units = "in" ,height = 8, width = 21, res=300)
  day9 <-ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y= Day9_melt$value)) +
  geom_point(size =3,na.rm=TRUE) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d") + 
  scale_y_continuous(limits = c(0,1)) + 
  geom_line(data=Day9_geno, aes(x=variable,y=value, group=Entry, color=Entry), size = 2)+  
  scale_color_manual(values=c("#6090ff", "#ff4c4c", "#55d655")) +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 45, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="none")
dev.off()
  
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/HorseRace_Day12.tiff", units = "in" ,height = 8, width = 21, res=300)
 ggplot(data=Day12_melt, aes(x=Day12_melt$variable, y= Day12_melt$value)) +
  geom_point(size =3,na.rm=TRUE) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 12d") + 
  scale_y_continuous(limits = c(0,1)) + 
  geom_line(data=Day6_clust, aes(x=Day6_clust$variable,y=Day6_clust$value), size = 2)+  
  #scale_color_manual(values=c("#6090ff", "#ff4c4c", "#55d655")) +
  theme_bw() +theme(text = element_text(size=15),axis.text.x = element_text(angle = 45, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="none")
dev.off()

ggplot(data=Day6_clust, aes(x=Day6_clust$variable, y= Day6_clust$value)) +
geom_line(data=Day6_clust, aes(x=Day6_clust$variable,y=Day6_clust$value), size = 1)
library("cowplot")
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/HorseRace_Combined.tiff", units = "in" ,height = 15, width = 21, res=300)
ggdraw() +
  draw_plot(day6, x = 0, y = 0, width = 1, height = .333) +
  draw_plot(day9, x = 0, y = .333, width = 1, height = .333) +
  draw_plot(day12, x = 0, y = .666667, width = 1, height = .333)
dev.off()
 