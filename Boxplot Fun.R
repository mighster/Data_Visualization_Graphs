# Description: Boxplot Fun
# by Kevin Falk
# May 2018


library(ggplot2)
library(digest)
library(reshape2)
library(plotly)
packageVersion('plotly')

######## 


df<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/All_Data_NoOutliers_Imputed.csv")
#subset only the 3 genotypes
df <- subset(df, df$Entry == "127" | df$Entry == "298" | df$Entry == "199")

#Switch names from Entry to Genotype A, B and C
df$Entry <- gsub("127", "A", df$Entry)
df$Entry <- gsub("199", "C", df$Entry)
df$Entry <- gsub("298", "B", df$Entry)

AllData <- df[, c(1:55)]

AllData$Entry=as.factor(AllData$Entry)
AllData$Day=as.factor(AllData$Day)
colnames(AllData)

PRLData <- AllData[c(6,5,20)] 
TRLData <- AllData[c(6,5,19)] 
SRData <- AllData[c(6,5,40)] 
DepthData <- AllData[c(6,5,35)] 
WidthData <- AllData[c(6,5,36)]
DepthWidthData <- AllData[c(6,5,37)]
AreaData <- AllData[c(6,5,43)]
VolumeData <- AllData[c(6,5,30)]
MSLData <- AllData[c(6,5,22)]
CMData <- AllData[c(6,5,48)]
CPData <- AllData[c(6,5,52)]
SRLLRB <- AllData[c(6,5,23)]
LED <- AllData[c(6,5,27)]


PRLData <- melt(PRLData, id.vars = 1:2)
TRLData <- melt(TRLData, id.vars = 1:2)
SRData <- melt(SRData, id.vars = 1:2)
DepthData <- melt(DepthData, id.vars = 1:2)
WidthData <- melt(WidthData, id.vars = 1:2)
AreaData <- melt(AreaData, id.vars = 1:2)
VolumeData <- melt(VolumeData, id.vars = 1:2)
DepthWidthData <- melt(DepthWidthData, id.vars = 1:2)
MSLData <- melt(MSLData, id.vars = 1:2)
CMData <- melt(CMData, id.vars = 1:2)
CPData <- melt(CPData, id.vars = 1:2)
SRLLRB <- melt(CPData, id.vars = 1:2)
LED <- melt(LED, id.vars = 1:2)

newdf <- rbind(TRLData, PRLData, SRData, WidthData, AreaData, LED)

DepthWidthData$Entry <-as.factor(DepthWidthData$Entry)
DepthWidthData$Day <-as.factor(DepthWidthData$Day)

newdf$Day <- factor(newdf$Day, c("6", "9", "12"))
newdf$variable <- factor(newdf$variable, labels = c("Total~Root~Length~(cm)","Primary~Root~Length~(cm)","Lateral~Root~Branches", "Width~(cm)","Area~(cm^2)","LED"))


tiff("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/Boxplot_fun.tiff", units = "in" ,height = 11, width = 11, res=300)
ggplot(data = newdf, aes(x=Entry, y=value, z = Day)) +
     geom_boxplot(size=1.25,aes(color = Entry)) +
     facet_wrap(~variable, scales = "free", ncol = 3, labeller = label_parsed) +
     scale_color_manual(values=c("#6090ff", "#ff4c4c", "#55d655"))+
     xlab("") +
     ylab("") +
     theme_bw() +
     theme(text = element_text(size=20), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(),legend.position="none")
dev.off()
 
tiff("Boxplot_fun.tiff", units = in ,height = 5, width = 5, res=300)
p <- ggplotly(p)

chart_link = plotly_POST(p, filename="geom_boxplot/time-series")
chart_link
