library(MASS)
library(ggplot2)
library(RCurl)
library(dplyr)
require(scales)
require(gridExtra)

##########                  https://www.r-bloggers.com/computing-and-visualizing-lda-in-r/
##########                  https://gist.github.com/thigm85/8424654
  
df1 <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/3 Genotypes/3 Genotypes all data.csv", header = T)
colnames(df1)
genos <- (df1[,c(6,7,18:56)])
metadata <- read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Cluster_Summary/Cluster_metadata.csv")


Geno3metadata <- metadata %>% filter(Entry == "127"|Entry == "199"|Entry == "298")


Day6_data <- subset(genos, genos$Day == "6")
Day9_data <- subset(genos, genos$Day == "9")
Day12_data <- subset(genos, genos$Day == "12")
Day6_Geno3metadata <- subset(Geno3metadata, Geno3metadata$Day == "6")
Day9_Geno3metadata <- subset(Geno3metadata, Geno3metadata$Day == "9")
Day12_Geno3metadata <- subset(Geno3metadata, Geno3metadata$Day == "12")
colnames(Day6_data)

day6 <- lda(formula = Entry ~ ., 
         data = Day6_data[,c(-1,-11)], 
         prior = c(1,1,1)/3)
day9 <- lda(formula = Entry ~ ., 
            data = Day9_data[,c(-1,-11)],
            prior = c(1,1,1)/3)
day12 <- lda(formula = Entry ~ ., 
            data = Day12_data[,c(-1,-11)],
            prior = c(1,1,1)/3)

prop.lda = day6$svd^2/sum(day6$svd^2)
prop.lda = day9$svd^2/sum(day9$svd^2)
prop.lda = day12$svd^2/sum(day12$svd^2)

plda6 <- predict(object = day6,
                newdata = Day6_data)

pca6 <- prcomp(Day6_data[,c(-1,-2,-11)],
              center = TRUE,
              scale. = TRUE) 

dataset6 = data.frame(Entry = Day6_data[,"Entry"],
                     pca = pca6$x, lda = plda6$x)

plda9 <- predict(object = day9,
                 newdata = Day9_data)

pca9 <- prcomp(Day9_data[,c(-1,-2,-11)],
               center = TRUE,
               scale. = TRUE) 

dataset9 = data.frame(Entry = Day9_data[,"Entry"],
                      pca = pca9$x, lda = plda9$x)

plda12 <- predict(object = day12,
                 newdata = Day12_data)

pca12 <- prcomp(Day12_data[,c(-1,-2,-11)],
               center = TRUE,
               scale. = TRUE) 

dataset12 = data.frame(Entry = Day12_data[,"Entry"],
                      pca = pca12$x, lda = plda12$x)

newdataset <- as.data.frame(rbind(cbind(c(6),dataset6$Entry,dataset6$lda.LD1,dataset6$lda.LD2),cbind(c(9),dataset9$Entry,dataset9$lda.LD1,dataset9$lda.LD2),cbind(c(12),dataset12$Entry,dataset12$lda.LD1,dataset12$lda.LD2)))
colnames(newdataset) <- c("Day","Entry","lda.LD1","lda.LD2")

LDA1 <- mean(c(0.96921713,0.94885266,0.8045564))
LDA2 <- mean(c(0.03078287,0.05114734,0.1954436))

tiff("C:/Users/falk/Google Drive/PhD/Papers/RSA ARIA Methods Paper - Zaki Vahid Kevin/Figures/LDA/LDA_Feb21.tiff", units = "in" ,height = 4, width = 10, res=200)
ggplot(newdataset) + geom_point(aes(lda.LD1, lda.LD2, colour = as.factor(Entry), shape = as.factor(Day)), size = 2.5) +
  ylab("LD Value") + xlab("LD Value") + 
  labs(x = paste("LD1 (", percent(LDA1), ")", sep=""),
       y = paste("LD2 (", percent(LDA2), ")", sep=""))+
  scale_color_manual(values=c("blue","red","green3"))+
  labs(color= "Entry", shape = "Day") +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1,vjust=0.5), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank(), legend.text.align =1)
dev.off()
