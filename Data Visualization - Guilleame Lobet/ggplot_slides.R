## ---- warning=F----------------------------------------------------------

library("tidyverse")


## ----load_libraries, warning=F-------------------------------------------
mydata <- read_csv("datasets/architect.csv")


## ---- warning=F----------------------------------------------------------
str(mydata)

## ---- warning=F----------------------------------------------------------
mydata %>%
  filter(genotype == "dense") %>%
  head()


## ---- warning=F----------------------------------------------------------

mydata %>%
  select(c(FileName, genotype, Time, Height, Width))%>%
  head()



## ---- warning=F----------------------------------------------------------

mydata <- mydata %>%
  select(-c(X1))

mydata %>%
  head()


## ---- warning=F----------------------------------------------------------

mydata %>%
  mutate(newvar = log(TRL))%>%
  select(c(genotype, Time, newvar)) %>%
  head()


## ---- echo=T, warning=F--------------------------------------------------
myplot <- ggplot(data=mydata, aes(x=TRL, y=TNLR))
myplot

## ---- echo=T, warning=F--------------------------------------------------

myplot + 
  ### <b>
  geom_point()
  ### </b>

## ---- echo=T, warning=F--------------------------------------------------
myplot + 
  ### <b>
  geom_line()
  ### </b>

## ---- echo=T, warning=F--------------------------------------------------
myplot + 
  ### <b>
  geom_step()
  ### </b>


## ---- echo=T, warning=F--------------------------------------------------
myplot + 
  ### <b>
  geom_point() + geom_step()
  ### </b>

## ---- echo=T, warning=F--------------------------------------------------

ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=genotype)) +
  geom_point()


## ---- echo=T, warning=F--------------------------------------------------

ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=Height)) +
  geom_point()


## ---- echo=T, warning=F--------------------------------------------------

ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=Time, shape=genotype)) +
  geom_point()


## ---- echo=T, warning=F--------------------------------------------------

ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=genotype)) +
  geom_point() + 
  facet_wrap(~ genotype)


## ---- echo=T, warning=F--------------------------------------------------


ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=genotype)) + 
  geom_point() + 
  geom_smooth()


## ---- echo=T, warning=F, message=F---------------------------------------
ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=genotype)) + 
  geom_point() + 
  geom_smooth(se = FALSE)


## ---- echo=T, warning=F, message=F---------------------------------------
ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=genotype)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method="lm")


## ---- echo=T, warning=F--------------------------------------------------
ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=genotype)) + 
  geom_point() + 
  stat_ellipse()


## ---- echo=T, warning=F--------------------------------------------------

ggplot(data=mydata, aes(x=genotype, y=TRL)) + 
  geom_boxplot()


## ---- echo=T, warning=F--------------------------------------------------
# Normilsation function
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}


## ---- echo=T, warning=F--------------------------------------------------

mydatalong <- mydata %>%
  gather(TRL:ExtPathLength, key = "variable", value = "value")

mydatalong %>%
  head()



## ---- echo=T, eval=F, warning=F------------------------------------------
## 
## # Make a lin plot, for each variable
## mydatalong %>%
##   filter(Time == max(Time)) %>%   # Select just one time point (the last)
##   ggplot(aes(genotype, value, colour=genotype)) +
##   geom_boxplot() +
##   facet_wrap(~variable, nrow=2)+
##   theme(text = element_text(size=9))

## ---- echo=F, eval=T, warning=F, out.width = '100%'----------------------

# Make a lin plot, for each variable
mydatalong %>%
  filter(Time == max(Time)) %>%   # Select just one time point (the last)
  ggplot(aes(genotype, value, colour=genotype)) + 
  geom_boxplot() + 
  facet_wrap(~variable, nrow=2)+ 
  theme(text = element_text(size=9))

## ---- echo=T, eval=F, warning=F------------------------------------------
## 
## # Make a lin plot, for each variable
## mydatalong %>%
##   filter(Time == max(Time)) %>%   # Select just one time point (the last)
##   ggplot(aes(genotype, value, colour=genotype)) +
##   geom_boxplot() +
##   facet_wrap(~variable, nrow=2, scales = "free")+
##   theme(text = element_text(size=9))
## 
## 

## ---- echo=F, eval=T, warning=F, out.width = '100%'----------------------

mydatalong %>%
  filter(Time == max(Time)) %>%   # Select just one time point (the last)
  ggplot(aes(genotype, value, colour=genotype)) + 
  geom_boxplot() + 
  facet_wrap(~variable, nrow=2, scales = "free")+ 
  theme(text = element_text(size=9))



## ---- warning=F, message=F-----------------------------------------------
library("plotly")

## ---- warning=F, message=F-----------------------------------------------


pl <- ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=genotype)) +
  geom_point()

ggplotly(pl)


## ---- warning=F, message=F-----------------------------------------------


pl <- ggplot(data=mydata, aes(x=TRL, y=TNLR, colour=genotype, label=FileName)) +
  geom_point()

ggplotly(pl)


## ---- warning=F, message=F-----------------------------------------------



pl <- mydata %>%
  filter(Time == max(Time)) %>%
  ggplot(aes(x=genotype, y=TRL, label=FileName)) +
  geom_boxplot()

ggplotly(pl )


