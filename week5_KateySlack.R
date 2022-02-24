#load packages
library(tidyr)
library(dplyr)
library(ggplot2)

#read in data
blooddat= read.csv("C:/Users/Katey Slack/Desktop/class stuff/QuantMethods/K.Slack/20_21Waterandblood.csv")

#looking at the structure
str(blooddat)

#real quick data clean
blooddat = rename(blooddat,Site = ï..Site)
list(blooddat$Site)
blooddat$Site[blooddat$Site == 'Buller Hatchery'|blooddat$Site =='buller']<- 'Buller' 
blooddat$Site[blooddat$Site == '762_Bridge']<- '762'
blooddat$Site[blooddat$Site == 'RiversideCC']<- 'Riverside'
blooddat$Site[blooddat$Site == 'TB']<-'Thomas Bridge'
list(blooddat$Site) 
bloodclean= select(blooddat, c(1, 2, 3, 4, 5, 6, 7, 8, 11, 14, 15, 18, 21, 22, 23, 24, 25, 26, 27, 28, 29))
list(bloodclean)
#Data is clean 

# Hypothesis 1: Hellbender blood metrics and dissolved oxygen will be negatively correlated

#Pearsons correlation 

pthct <- cor.test(bloodclean$Avg.daily.DO,bloodclean$Mean_T0Hct)
pthct

pthb <- cor.test(bloodclean$Avg.daily.DO,bloodclean$Mean_T0HB)
pthb
#Found a negative correlation and a superlow , which makes sense because dissolved oxygen increases across season and baseline blood metrics decrease 

# Hypothesis 2: Do blood metrics differ between males exibiting parental care and males not maintaining nests?

#filter nesters and nonnesters 2021 in 2021
blood21=filter(bloodclean, Season == '2021' & sampling.day != 'oviposition')
#I have different outgroups at midincubation and at hatching so I am going to make two subsets for each
OGM= filter(blood21, sampling.day == "midincubation" )
OGH= filter(blood21, sampling.day == "hatching")

##looking at data distribution with box plots
#T0 and Delta hemoglobin @ mid incubation
OGMhb=ggplot(OGM, aes(x=Status, y= Mean_T0HB))+
  geom_point(aes(Status,jitter(Mean_T0HB,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)
OGMDhb=ggplot(OGM, aes(x=Status, y= D_HB))+
  geom_point(aes(Status,jitter(D_HB,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)
#T0 and Delta hematocrit @midincubation
OGMhct=ggplot(OGM, aes(x=Status, y= Mean_T0Hct))+
  geom_point(aes(Status,jitter(Mean_T0Hct,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)
OGMDhct=ggplot(OGM, aes(x=Status, y= D_Hct))+
  geom_point(aes(Status,jitter(D_Hct,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)

#T0 and Delta hemoglobin @ hatching
OGHhb=ggplot(OGH, aes(x=Status, y= Mean_T0HB))+
  geom_point(aes(Status,jitter(Mean_T0HB,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)
OGHDhb=ggplot(OGH, aes(x=Status, y= D_HB))+
  geom_point(aes(Status,jitter(D_HB,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)
#T0 and Delta hematocrit @ hatching
OGHhct=ggplot(OGH, aes(x=Status, y= Mean_T0Hct))+
  geom_point(aes(Status,jitter(Mean_T0Hct,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)
OGHDhct=ggplot(OGH, aes(x=Status, y= D_Hct))+
  geom_point(aes(Status,jitter(D_Hct,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)
#the box plots are showing me that there are some differences in distribution 
#Realizing I bit off more than I can chew
#For simplicity purposes, I am going to do a permutation for the data that showed the greatest differences on the box plots

#Building a permutation for Delta Hematocrit @ hatching
bloodperm1=select(OGH, 'Status', 'D_Hct')
#random number generator
set.seed(101)
#create a place to store results
res1 <- NA

sample(bloodperm1,2, replace = F) 

##permutation tests####
##creating a for loop
for (i in 1:10000){
  hctboot<-sample(bloodperm1$D_Hct)
  nestboot<-hctboot[1:length(bloodperm1$Status[bloodperm1$Status=="nester"])]
  nonnestboot<-hctboot[(length(bloodperm1$Status[bloodperm1$Status=="nonnester"])+1):length(bloodperm1$Status)]
  res1[i]<-mean(nestboot)-mean(nonnestboot)
}

#observed mean difference
obs1<-mean(bloodperm1$D_Hct[bloodperm1$Status=="nester"])-mean(bloodperm1$D_Hct[bloodperm1$Status=="nonnester"])
#I think this is right? 


#histogram to look at distribution
hist(res1,col="gray",las=1,main="")
abline(v=obs1,col="red")

#looking at values
res1
obs1

#calculating p value
res1[res1>=obs1]
length(res1[res1>=obs1])
0/10000
mean(res1>=obs1) 
#every thing was zero so I don't know if I did it right :/

#I am going to run some additional tests to see if its significant 

install.packages("coin")
library(coin)
bloodperm1$Status=as.factor(bloodperm1$Status)

oneway_test(D_Hct~Status,data=bloodperm1)

oneway_test(D_Hct~Status,data=bloodperm1,distribution="exact")

oneway_test(D_Hct~Status,data=bloodperm1,distribution=approximate(nresample=9999))
#tests gave me really similar p values on the cusp of statistical clarity :)

#welch's T
wtthct <- t.test(D_Hct~Status,data=bloodperm1)
wtthct
#This one gave me a more significant pvalue

#student T
tthct <- t.test(D_Hct~Status,data=bloodperm1,var.equal=TRUE)
tthct 
#this one gave me a p value closer to the permutation 

##based on p-values, there seems to be a difference in the change in hematocrit from T0 to T60 between nesters and nonnesters.



#found that my pvalue is higher when equal variance is included
