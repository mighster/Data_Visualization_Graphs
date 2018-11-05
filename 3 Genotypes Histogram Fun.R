# Description: Histogram Fun
# by Kevin Falk
# May 2018


library(ggplot2)
library(digest)
library(reshape2)
######## 

AllData=read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/3 Genotypes/3 Genotypes all data.csv")

AllData[1:5,20:23]
AllData$Entry=as.factor(AllData$Entry)

PRLData <- AllData[c(6,5,20)] 
TRLData <- AllData[c(6,5,19)] 
SRData <- AllData[c(6,5,47)] 
DepthData <- AllData[c(6,5,42)] 
WidthData <- AllData[c(6,5,43)]
DepthWidthData <- AllData[c(6,5,44)]
AreaData <- AllData[c(6,5,50)]
VolumeData <- AllData[c(6,5,29)]


PRLData <- melt(PRLData, id.vars = 1:2)
TRLData <- melt(TRLData, id.vars = 1:2)
SRData <- melt(SRData, id.vars = 1:2)
DepthData <- melt(DepthData, id.vars = 1:2)
WidthData <- melt(WidthData, id.vars = 1:2)
AreaData <- melt(AreaData, id.vars = 1:2)
VolumeData <- melt(VolumeData, id.vars = 1:2)
DepthWidthData <- melt(DepthWidthData, id.vars = 1:2)

newdf <- rbind(PRLData,TRLData, SRData, DepthData, WidthData,AreaData, VolumeData, DepthWidthData)

PRL <- ggplot(data = PRLData, aes(PRLData$value)) + geom_histogram(aes(fill=Entry), binwidth = 3, position = position_stack(reverse = TRUE)) +
  coord_flip() + facet_grid(~Day) + theme_bw() + scale_x_reverse() + labs(y = "Total Root Length (cm)", x = "")
TRL <- ggplot(data = TRLData, aes(TRLData$value)) + geom_histogram(aes(fill=Entry), binwidth = 25) + facet_grid(~Day) + theme_bw() + labs(x = "Total Root Length (cm)", y = "")
SR <- ggplot(data = SRData, aes(SRData$value)) + geom_histogram(aes(fill=Entry), binwidth = 10)+ facet_grid(~Day) + theme_bw() + labs(x = "Secondary Roots", y = "")
Depth <- ggplot(data = DepthData, aes(DepthData$value)) + geom_histogram(aes(fill=Entry), binwidth = 3,  position = position_stack(reverse = TRUE))+ coord_flip()+ facet_grid(~Day) + theme_bw() + scale_x_reverse() + labs(y = "Depth (cm)", x = "")
Width <- ggplot(data = WidthData, aes(WidthData$value)) + geom_histogram(aes(fill=Entry), binwidth = 2)+ facet_grid(~Day) + theme_bw() + labs(x = "Width (cm)", y = "")
Area <- ggplot(data = AreaData, aes(AreaData$value)) + geom_histogram(aes(fill=Entry), binwidth = 10)+ facet_grid(~Day) + theme_bw() + labs(x = "Area Area (cm-2)", y = "")
Depth_Width <- ggplot(data = DepthWidthData, aes(DepthWidthData$value)) + geom_histogram(aes(fill=Entry), binwidth = 2)+ facet_grid(~Day) + theme_bw() + labs(x = "Depth:Width", y = "")

ggplot(data = newdf, aes(x= variable, y=value) + geom_boxplot())


ggplot(data = newdf, aes(x=variable, y=value, (fill=Entry) + geom_boxplot(outlier.color = 'black', outlier.shape = 16) + facet_wrap(~Day,nrow=3)))

ggplot(newdf,aes(variable,value,fill=as.factor(Entry)))+
  geom_bar(position="dodge",stat = "identity")+
  facet_wrap(~variable,nrow=3)
       
ggplot(newdf, aes(x=variable, y=value, fill=Entry)) +
  geom_boxplot() + facet_wrap(~Day,nrow=3)

ggplot(newdf, aes(x=variable, y=value, fill=Entry)) +
  geom_boxplot() + facet_wrap(~variable,nrow=3)


multiplot(TRL, PRL, SR, Depth, Depth_Width, Area, cols=3, nrows =3)

0

ggplot



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
