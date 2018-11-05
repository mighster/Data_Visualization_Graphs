# Description: Lateral Root Histogram Fun
# by Kevin Falk
# May 2018


library(ggplot2)
library(digest)
library(reshape2)
######## 

AllData=read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/FIgures/SecondaryRootMelt.csv")

AllData$Entry=as.factor(AllData$Entry)
AllData$Day=as.factor(AllData$Day)
AllData[1:2,1:2]
SRData<-AllData

SRData <- AllData[c(1,2,19)] 

SRData <- melt(SRData, id.vars = 1:2)

colors <- c()

ggplot(data = SRData, aes(SRData$value)) + geom_histogram(aes(fill=Entry), binwidth = 15)+ facet_grid(~Day) + theme_bw() + labs(x = "Secondary Roots", y = "")

ggplot(data = SRData, aes(SRData$value)) + geom_histogram(aes(fill=Entry), binwidth = 5,(scale_color_manual(values = c("Blue","Green","Red")))) + facet_grid(~Day) + theme_bw() + labs(x = "Secondary Roots", y = "")

(scale_color_manual(values = c("Blue","Green","Red")))
                                                          
                                                          
write.csv(SRData,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/SecondaryRootMelt.csv", row.names = F)
