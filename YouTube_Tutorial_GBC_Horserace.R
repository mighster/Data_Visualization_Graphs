library(reshape2)
library(ggplot2)
library(dplyr)

#Import new df
AllData <-read.csv("C:/Users/falk/Google Drive/PhD/YouTube Tutorials/GBC_HorseRace_tutorial.csv", header = T)
colnames(AllData)
AllData$Cluster <- as.factor(AllData$Cluster) #set all clusters as factor
colnames(AllData[3]) <- "Cluster"
summary(AllData$Cluster) #double check our cluster variable
#############################################################################################################
######################### Day 9 Extracting MEANS from all data ##############################################
#############################################################################################################

phenotypic_data_scaled <- scale(as.matrix(AllData[,c(4:52)])) # scale all phenotypic data around 0, between [-4,+4]
phenotypic_data_scaled[1:10,1:10] #double check our scaled variables
scaled_df <- as.data.frame(cbind(as.data.frame(AllData[3]),phenotypic_data_scaled)) #re-attach our cluster variable
scaled_df[1:30,1:10] #double check our df
#Select which phenotypic traits you wish to display
scaled_df <- scaled_df %>% dplyr::select(Cluster,TRL,PRL,WID,CVA,LRB,VOL,LRA,SOL,LED,RHZO,TRL_GR,TRLUpper,Root_weight) 
scaled_df[1:10,1:10] #double check our df

#Calculate MEAN value for each of the 8 clusters for the line graph
Cluster_Means <- scaled_df %>% group_by(Cluster) %>% 
                                        dplyr::summarize(TRL=mean(TRL),PRL=mean(PRL),WID=mean(WID),CVA=mean(CVA),LRB=mean(LRB),
                                                        VOL=mean(VOL),LRA=mean(LRA),SOL=mean(SOL),LED=mean(LED),RHZO=mean(RHZO),
                                                        TRL_GR=mean(TRL_GR),TRLUpper=mean(TRLUpper),Root_weight=mean(Root_weight))
Cluster_Means[,1:10] 
#############################################################################################################
###################################### MELT Just Normal Traits ALL DATA  ####################################
#############################################################################################################
#reshape main dataframe using melt function for use in graph
melted_df <-  melt(scaled_df, id.vars = "Cluster") #Reshape using the CLUSTER column
melted_df[1:10,]
melted_df$Cluster <- as.factor(melted_df$Cluster) #set cluster as factor

#############################################################################################################
########################################### Just Normal Traits CLUSTERS  ####################################
#############################################################################################################
#reshape cluster means dataframe using melt function for use in graph
Cluster_melt <-  melt(Cluster_Means, id.vars = "Cluster") #Reshape using the CLUSTER column
Cluster_melt$Cluster <- as.factor(Cluster_melt$Cluster) #set cluster as factor
Cluster_melt[1:10,]

#############################################################################################################
########################################### Create GRAPH  ###################################################
#############################################################################################################
ggplot(data=melted_df, aes(x=melted_df$variable, y=melted_df$value, color=Cluster)) +
  scale_fill_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Cluster_melt, aes(x=Cluster_melt$variable,y=Cluster_melt$value, group=Cluster, color=Cluster), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())

ggplot(data=melted_df, aes(x=melted_df$variable, y=melted_df$value)) +
  geom_point(size =0.85,na.rm=TRUE,position=position_jitter(w=0.15)) +
  ylab("Normalized BLUP Value") + xlab("RSA Trait at 9d")  +
  scale_y_continuous(limits = c(-4,4)) +
  geom_line(data=Cluster_melt, aes(x=Cluster_melt$variable,y=Cluster_melt$value, group=Cluster, color=Cluster), size = 1.5) +
  theme_bw() +
  scale_color_manual(values=c("blue","red","orange2","green3","black","yellow2","magenta","cyan")) +
  labs(color=expression(atop('Genotypic', 'Cluster'))) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 90, hjust=1), panel.background = element_blank(), panel.grid = element_blank(), strip.background = element_blank())
