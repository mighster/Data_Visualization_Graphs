library(reshape2)
library(ggplot2)
library(RCurl)
library(dplyr)


#Import new df
input_df <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Correlations/Day9_traitCorrelations.csv")
colnames(input_df)[1] <- "trait"

str(input_df)
heatmap(new_df)

df <- as.matrix(input_df)
postmelt <- melt(input_df, id.vars = c("trait"))

new_df <- as.matrix(postmelt)

ggplot(data=postmelt, aes(x=variable, y=value)) +
  geom_tile(aes(colour = "white")) + 
  scale_fill_gradient(low = "white", high = "steelblue")

library(ggplot2)
ggplot(data = postmelt, aes(x=trait, y=variable, fill=value)) + 
  geom_tile()+
scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, limit = c(-1,1))
