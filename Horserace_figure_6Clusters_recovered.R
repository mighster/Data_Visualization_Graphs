library(reshape2)
library(ggplot2)
library(RCurl)
library(dplyr)

#create function to identify maximum value in a column
colMax <- function(data) sapply(data, max, na.rm = TRUE)
#create function to identify minimum value in a column
colMin <- function(data) sapply(data, min, na.rm = TRUE)

#Import new df
df <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2037 Data - Growth Chamber/Data4Paper/Adjusted_BLUPs_NoBad.csv")

#remove BAD genotypes PI594438 (273) PI495832 (202) PI467333A (382) PI473899 (390) PI506528 (204) PI445845 (367)
df <- df %>%
  filter(!((Entry == "273")|(Entry == "202")|(Entry  == "382")|( df$Entry  == "390")|( df$Entry  == "204")|( df$Entry  == "367" )))

#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day32_data <- subset(df, df$Day == "32")

Day6_cluster3 <- subset(Day6_data, Day6_data$Cluster == "3")
Day6_cluster2 <- subset(Day6_data, Day6_data$Cluster == "2")
Day6_cluster3 <- subset(Day6_data, Day6_data$Cluster == "3")
Day6_cluster3 <- subset(Day6_data, Day6_data$Cluster == "3")
Day6_cluster3 <- subset(Day6_data, Day6_data$Cluster == "3")
Day6_cluster3 <- subset(Day6_data, Day6_data$Cluster == "3")




cluster_means <- data.frame(matrix(NA, nrow=6, ncol = 39))
cluster3 <- subset(Day6_data[,6:45], Day6_data$Cluster == "3")
cluster_means <- apply(cluster3, 2, mean)
cluster2 <- subset(Day6_data[,6:45], Day6_data$Cluster == "2")
cluster2 <- apply(cluster2, 2, mean)
cluster2 <- apply(cluster2, mean, na.rm = TRUE)
cluster3 <- subset(Day6_data[,6:45], Day6_data$Cluster == "3")
cluster3 <- sapply(cluster3, mean, na.rm = TRUE)
cluster4 <- subset(Day6_data[,6:45], Day6_data$Cluster == "4")
cluster4 <- sapply(cluster4, mean, na.rm = TRUE)
cluster5 <- subset(Day6_data[,6:45], Day6_data$Cluster == "5")
cluster5 <- sapply(cluster5, mean, na.rm = TRUE)
cluster6 <- subset(Day6_data[,6:45], Day6_data$Cluster == "6")
cluster6 <- sapply(cluster6, mean, na.rm = TRUE)

tapply(Day6_data$TRL, Day6_data$Cluster, mean)

#take only the columns with numerical data
colnum=c(6:45) 

#Create empty data frame for BLUP output
Day6NormalizedOutput <- data.frame(matrix(vector(),294,3, dimnames=list(c(), c("Entry"))))
Entry <- Day6_data$Entry
Day6NormalizedOutput$Entry <- Entry
Day6NormalizedOutput$Day <- c(6)
Cluster <- Day6_data$Cluster
Day6NormalizedOutput$Cluster <- Cluster

Day6ColMax <- colMax(Day6_data[,colnum])
Day6ColMin <- colMin(Day6_data[,colnum])
Day6ColSpread <- Day6ColMax-Day6ColMin

#remove row information
Day6_numbers <- Day6_data[6:45]
str(Day6_numbers)

colnum=c(3:45) 

for (i in 3:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_numbers)[x] #sets the current column header as the trait name
  Day6ColMax <- colMax(Day6_numbers[x])
  Day6ColMin <- colMin(Day6_numbers[x])
  Day6ColSpread <- Day6ColMax-Day6ColMin
  newcol <- (Day6_numbers[x] - Day6ColMin) / Day6ColSpread
  Day6NormalizedOutput <- cbind(Day6NormalizedOutput,newcol)
}

#Create empty data frame for BLUP output
Day9NormalizedOutput <- data.frame(matrix(vector(),294,3, dimnames=list(c(), c("Entry"))))
Entry <- Day9_data$Entry
Day9NormalizedOutput$Entry <- Entry
Day9NormalizedOutput$Day <- c(9)
Cluster <- Day9_data$Cluster
Day9NormalizedOutput$Cluster <- Cluster

#remove row information
Day9_numbers <- Day9_data[6:45]
colnum=c(3:45) 
for (i in 3:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_numbers)[x] #sets the current column header as the trait name
  Day9ColMax <- colMax(Day9_numbers[x])
  Day9ColMin <- colMin(Day9_numbers[x])
  Day9ColSpread <- Day9ColMax-Day9ColMin
  newcol <- (Day9_numbers[x] - Day9ColMin) / Day9ColSpread
  Day9NormalizedOutput <- cbind(Day9NormalizedOutput,newcol)
}

#Create empty data frame for BLUP output
Day32NormalizedOutput <- data.frame(matrix(vector(),294,3, dimnames=list(c(), c("Entry"))))
Entry <- Day32_data$Entry
Day32NormalizedOutput$Entry <- Entry
Day32NormalizedOutput$Day <- c(32)
Cluster <- Day32_data$Cluster
Day32NormalizedOutput$Cluster <- Cluster

#remove row information
Day32_numbers <- Day32_data[6:45]
colnum=c(3:45) 
for (i in 3:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day32_numbers)[x] #sets the current column header as the trait name
  Day32ColMax <- colMax(Day32_numbers[x])
  Day32ColMin <- colMin(Day32_numbers[x])
  Day32ColSpread <- Day32ColMax-Day32ColMin
  newcol <- (Day32_numbers[x] - Day32ColMin) / Day32ColSpread
  Day32NormalizedOutput <- cbind(Day32NormalizedOutput,newcol)
}

NormalizedOutput <- rbind(Day6NormalizedOutput,Day9NormalizedOutput,Day32NormalizedOutput)
write.csv(NormalizedOutput, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/NormalizedBLUPsAllDays_294.csv")


#Import new df
#data<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2037 Data - Growth Chamber/Z-score plots/z-Scores_day6_MG2.csv")

#LETS ONLY GRAPH RSA TRAITS THAT ARE BIOLOGICALLY WORTHY
GraphingData <- NormalizedOutput[,c(3:3,5:33,35)]
#MELT it!
postmelt <- melt(GraphingData, id.vars = c("Entry","Day","Cluster"))


#split data into df Days
Day6_melt <- subset(postmelt, postmelt$Day == "6")
Day9_melt <- subset(postmelt, postmelt$Day == "9")
Day32_melt <- subset(postmelt, postmelt$Day == "32")

cluster_subset <- subset(postmelt, Cluster == "3" | Entry == "298" | Entry == "399")
#Switch names from Entry to Genotype A, B and C
geno$Entry <- gsub("327", "A", geno$Entry)
geno$Entry <- gsub("399", "C", geno$Entry)
geno$Entry <- gsub("298", "B", geno$Entry)

#split data into df Days
Day6_geno <- subset(geno, geno$Day == "6")
Day9_geno <- subset(geno, geno$Day == "9")
Day32_geno <- subset(geno, geno$Day == "32")

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2037 Data - Growth Chamber/Data4Paper/HorseRace_Day6.tiff", units = "in" ,height = 8, width = 23, res=300)
  day6 <- ggplot(data=Day6_melt, aes(x=Day6_melt$variable, y= Day6_melt$value)) +
  geom_point(size =3,na.rm=TRUE) +
    ylab("Normalized BLUP Value") + xlab("RSA Trait at 6d") + 
    scale_y_continuous(limits = c(0,3)) + 
  geom_line(data=Day6_geno, aes(x=variable,y=value, group=Entry, color=Entry), size = 2)+  
    scale_color_manual(values=c("#6090ff", "#ff4c4c", "#55d655")) +
    theme_bw() +
    theme(text = element_text(size=35),axis.text.x = element_text(angle = 45, hjust=3), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="none")
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2037 Data - Growth Chamber/Data4Paper/HorseRace_Day9.tiff", units = "in" ,height = 8, width = 23, res=300)
  day9 <-ggplot(data=Day9_melt, aes(x=Day9_melt$variable, y= Day9_melt$value)) +
  geom_point(size =3,na.rm=TRUE) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d") + 
  scale_y_continuous(limits = c(0,3)) + 
  geom_line(data=Day9_geno, aes(x=variable,y=value, group=Entry, color=Entry), size = 2)+  
  scale_color_manual(values=c("#6090ff", "#ff4c4c", "#55d655")) +
  theme_bw() +
  theme(text = element_text(size=35),axis.text.x = element_text(angle = 45, hjust=3), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="none")
dev.off()
  
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2037 Data - Growth Chamber/Data4Paper/HorseRace_Day32.tiff", units = "in" ,height = 8, width = 23, res=300)
 day32<- ggplot(data=Day32_melt, aes(x=Day32_melt$variable, y= Day32_melt$value)) +
  geom_point(size =3,na.rm=TRUE) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 32d") + 
  scale_y_continuous(limits = c(0,3)) + 
  geom_line(data=Day32_geno, aes(x=variable,y=value, group=Entry, color=Entry), size = 2)+  
  scale_color_manual(values=c("#6090ff", "#ff4c4c", "#55d655")) +
  theme_bw() +theme(text = element_text(size=35),axis.text.x = element_text(angle = 45, hjust=3), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="none")
dev.off()

library("cowplot")
tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2037 Data - Growth Chamber/Data4Paper/HorseRace_Combined.tiff", units = "in" ,height = 35, width = 23, res=300)
ggdraw() +
  draw_plot(day6, x = 0, y = 0, width = 3, height = .333) +
  draw_plot(day9, x = 0, y = .333, width = 3, height = .333) +
  draw_plot(day32, x = 0, y = .666667, width = 3, height = .333)
dev.off()
 