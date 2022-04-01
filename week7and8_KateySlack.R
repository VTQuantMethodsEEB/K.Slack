##Week 7##


## read in data
blooddat=read.csv("20_21Waterandblood.csv")

##load packages
library(dplyr)
library(ggplot2)
library(tidyr)
#clean data
blooddat = rename(blooddat,Site = ï..Site)
list(blooddat$Site)
blooddat$Site[blooddat$Site == 'Buller Hatchery'|blooddat$Site =='buller']<- 'Buller' 
blooddat$Site[blooddat$Site == '762_Bridge']<- '762'
blooddat$Site[blooddat$Site == 'RiversideCC']<- 'Riverside'
blooddat$Site[blooddat$Site == 'TB']<-'Thomas Bridge'
list(blooddat$Site) 
bloodclean= select(blooddat, c(1, 2, 3, 4, 5, 6, 7, 8, 11, 14, 15, 18, 21, 22, 23, 24, 25, 26, 27, 28, 29))
list(bloodclean)

##1- Univariate linear model for one hypotheses

##Hypothesis: Is environmental dissolved Oxygen and baseline hematocrit related?

#create linear models using continuous variables for both predictor and response

DOHct= lm(Mean_T0Hct~Avg.daily.DO, data=bloodclean)
summary(DOHct)

#Y(baseline hematocrit)= -7.22X +103.09 
#pvalue indicates that dissolved oxygen has a strong influence on baseline hematocrit

##2- check assumptions of linearity 
par(mfrow=c(2,2))
plot(DOHct)

resid(DOHct)
hist(resid(DOHct)) #shows nice bell curve of normality
shapiro.test(resid(DOHct)) #p-value =0.41

##3- annotate the results of diagnostics

#heteroscedasticity?? 
#-Residuals vs Fitted and Scale-Location both plot a straight line and the points are randomly scattered around the line

#normality??
#- Histogram of my residuals shows a nice bell curve, Normal Q-Q plot show strong normality, shapiro-wilk test results indicate normality 

#leverage points??
#-the residuals vs leverage plot reveals a few outliars, but the outliars do not appear to significantly skew the data 

##4- plot relationship in ggplot using stat_smooth/stat_summary

PDOHct=ggplot(data=bloodclean, aes(x=Avg.daily.DO, y=Mean_T0Hct))+ 
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=10),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.title=element_blank())
print(PDOHct)



##Week 8##

#cleanup columns
bloodclean = rename(bloodclean, wounds = Open.Wounds..Y.N..)
bloodclean2= dplyr::select(bloodclean, -c(6,7))
#1. Make a linear model with one or more variables for one of my hypotheses, articulate which Hypothesis I am testing.

#Hypothesis: Will baseline blood metrics change across the parental care period (from oviposition to emergence)?
#Oviposition of Hellbender eggs occurs in late august/early September and males with successful nests stay with the eggs through hatching(in November) until juveniles emmerge from the nest in spring. 
#I want to see if baseline hematocrit is influenced by the parental care period. 

#making sampling day a factor
bloodclean2$sampling.day=as.factor(bloodclean2$sampling.day)

lmday= lm(Mean_T0Hct~sampling.day, data=bloodclean2)
bloodclean2$sampling.day=relevel(bloodclean2$sampling.day, ref= "oviposition")
summary(lmday)
#using emmeans
library(emmeans)
library(multcompView)
library(multcomp)

emmeans(lmday, specs = "sampling.day", contr = "pairwise")

#my results show that there are some significant differences between different sampling periods. 
#In my summary, Emergence p-value suggest their might be a subtle difference compared to oviposition. There was a significant difference between oviposition and hatching. However, there was not a significant difference between oviposition and midincubation.
#Using the emmeans package to contrast sampling dates, baseline metrics at oviposition significantly differed from metrics at hatching. Emergence metrics significantly differed from hatching metrics and hatching metrics significantly differed from midincubation. 

#2. Use an interactive and additive model, explain hypothesis of each, and interpret R output. (use emmeans, effects, relevel, or predict)

#Hypothesis: Will parental care influence baseline blood metrics in addition to seasonal effects? 
#I want to compare hellbenders engaging in parental care and non-nesting out groups across the season.

lmday2= lm(Mean_T0Hct~Status+sampling.day, data=bloodclean2)
bloodclean2$sampling.day=relevel(bloodclean2$sampling.day, ref= "oviposition")
summary(lmday2)
emmeans(lmday2,specs = ~Status+sampling.day)

#using effects package
install.packages("effects")
library(effects)
plot(allEffects(lmday2))
#my output indicates that the only statistically significant interaction between nesting status and season occurs between nesters at oviposition and non-nesters at hatching
#the plots show a large difference at hatching. While there might be a difference in means between nesters and nonnesters, nonnesters have a lot of variability

#Hypothesis: Does the change in hematocrit between T0 and T60 depend on the mass and presence of open wounds?
lmwounds= lm(D_Hct~Mass.g.*wounds, data=bloodclean2)
summary(lmwounds)
plot(allEffects(lmwounds))
#output indicates no significant relationship, but the plot shown seems to show some sort of relationship. There appears to be a lot more variability in hellbenders with open wounds.


#3. Plot model(use predict) and overlay the model over the data

#plotting additive model
#new data frame
bloodclean2$Status= as.factor(bloodclean2$Status) #clean
pp <- with(bloodclean2,expand.grid(sampling.day=unique(sampling.day),
                                   Status=unique(Status)))
pp$hct <- predict(lmday2,newdata=pp)

addplot= ggplot(pp,aes(x=sampling.day,y=hct,colour=Status))+
  geom_point()+
  geom_line(aes(group=Status))+
  geom_point(data=bloodclean2, aes(x=sampling.day,y=Mean_T0Hct,colour = Status))
#The plot shows non-nesters have lower baseline hematocrit but not enough to significantly differ from nests because there is so much variation


##plotting the interactive model

#trying  yhat and predict
bloodclean2$yhat= predict(lmwounds) #predict function will not work for this line

#trying to create new dataframe for predictions
new.dat.combos <- with(bloodclean2, 
                       expand.grid(wounds=unique(wounds), 
                                   Mass.g.=seq(min(Mass.g.),max(Mass.g.), by=1))) #getting an error about "finite numbers"

#if my code worked then this I would try to predict the delta hematocrit using the new data frame/combos
new.dat.combos$hct <- predict(lmwounds,newdata=new.dat.combos)

###plotting prediction + data 
intplot= ggplot(new.dat.combos,aes(x=Mass.g.,y=D_Hct,colour=wounds))+ #set up plot using predictions dataset
  geom_line(aes(group=wounds))+ #draw lines that are predictions
  geom_point(data=bloodclean2, aes(x=Mass.g.,y=D_Hct,colour = wounds)) #add the observed data to the plot


