library(reshape2)
library(ggplot2)

premelt<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Root Angles/RTA_raw_data.csv", header = T, check.names=FALSE)

colnames(premelt)
MG_Data  <- premelt[,c(3,6,10:54)]

MG_Data$MG=as.factor(MG_Data$MG)  
MG_Data$Day=as.factor(MG_Data$Day)

colnames(MG_Data)
str(MG_Data$MG)
str(MG_Data$Day)

postmelt <- melt(MG_Data, id.vars = c("Day","MG"))

#write.csv(postmelt,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Root Angles/Root_Angle_Tally_melted.csv", row.names = F)

Day6_data <- subset(postmelt, postmelt$Day == "6")
Day9_data <- subset(postmelt, postmelt$Day == "9")
Day12_data <- subset(postmelt, postmelt$Day == "12")

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Root Angles/RTA_MG_Day6.tiff", units = "in" ,height = 8, width = 14, res=200)
ggplot(data=Day6_data, aes(x=Day6_data$variable, y=Day6_data$value)) +
  geom_smooth(data=Day6_data, aes(x=Day6_data$variable,y=Day6_data$value, group=MG, color=MG), size = 2)  +
  #scale_colour_gradientn(colours=rainbow(5)) +
  scale_x_discrete(breaks=seq(0, 90, 10)) +
  ylab("Mean Count") + xlab("Root Angle (0-90 degrees) of Various Countries at 6d") + 
  theme_bw() +
  theme(text = element_text(size=12),axis.text.x = element_text(angle = 45, hjust=1), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="right")
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Root Angles/RTA_MG_Day9.tiff", units = "in" ,height = 8, width = 14, res=200)
ggplot(data=Day9_data, aes(x=Day9_data$variable, y=Day9_data$value)) +
  geom_smooth(data=Day9_data, aes(x=Day9_data$variable,y=Day9_data$value, group=MG, color=MG), size = 2)  +
  #scale_colour_gradientn(colours=rainbow(5)) +
  scale_x_discrete(breaks=seq(0, 90, 10)) +
  ylab("Mean Count") + xlab("Root Angle (0-90 degrees) of Various Countries at 9d") + 
  theme_bw() +
  theme(text = element_text(size=12),axis.text.x = element_text(angle = 45, hjust=1), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="right")
dev.off()

tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Root Angles/RTA_MG_Day12.tiff", units = "in" ,height = 8, width = 14, res=200)
ggplot(data=Day12_data, aes(x=Day12_data$variable, y=Day12_data$value)) +
  geom_smooth(data=Day12_data, aes(x=Day12_data$variable,y=Day12_data$value, group=MG, color=MG), size = 2)  +
  #scale_colour_gradientn(colours=rainbow(5)) +
  scale_x_discrete(breaks=seq(0, 90, 10)) +
  ylab("Mean Count") + xlab("Root Angle (0-90 degrees) of Various Countries at 12d") + 
  theme_bw() +
  theme(text = element_text(size=12),axis.text.x = element_text(angle = 45, hjust=1), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="right")
dev.off()

