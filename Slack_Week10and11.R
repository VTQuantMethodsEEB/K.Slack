## Week 10 ## GLMs

#read in data and clean
blooddat=read.csv("20_21Waterandblood.csv")

##load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)
library(effects)
library(emmeans)
library(AER)
#clean data
blooddat = rename(blooddat,Site = ï..Site)
blooddat = rename(blooddat,wounds = Open.Wounds..Y.N..)
blooddat$Date= as.Date(blooddat$Date, "%m/%d/%Y")
list(blooddat$Site)
blooddat$Site[blooddat$Site == 'Buller Hatchery'|blooddat$Site =='buller']<- 'Buller' 
blooddat$Site[blooddat$Site == '762_Bridge']<- '762'
blooddat$Site[blooddat$Site == 'RiversideCC']<- 'Riverside'
blooddat$Site[blooddat$Site == 'TB']<-'Thomas Bridge'
list(blooddat$Site) 
bloodclean= select(blooddat, c(1, 2, 3, 4, 8, 11, 14, 15, 18, 21, 22, 23, 24, 25, 26, 27, 28, 29))
list(bloodclean)
bloodclean= na.omit(bloodclean)
library(car)
#1) Make a GLM (with one or more variable) for a hypothesis. Articulate the hypothesis. 

#Hypothesis: Are baseline blood metrics predicted by the number of leeches attached across the season?
#Within our system, Hellbenders harbor leeches which are vectors for blood feeding protozoan parasites, trypanosomes. When sampling, we count the number of leeches on the hellbender. 
#I would like to see if the effect of leeches on baseline hematocrit depend on sampling day?
# 
## interactive model looking at the number of leeches 
glm1= glm(Mean_T0Hct~Leeches_Total*sampling.day,data=bloodclean, family="poisson");
summary(glm1)

#this is count data with many zeros so I am checking for over dispersion
dispersiontest(glm1)
#my dispersion test showed my dispersion was 1.24 which is >1 so I am going to use a negative binomial 
glm2 = glm.nb(Mean_T0Hct~Leeches_Total*sampling.day, data=bloodclean)
summary(glm2) 
#comparing variable interactions
plot(allEffects(glm2))
#2) Explain the R output in relation to the hypothesis (use emmeans, effect, relevel, and predict). Include the explanation in the code.

#my results show that the number of total leeches attached is not significant in the the model. Of the sampling days, baseline hematocrit at hatching is significantly different from the other sampling periods.
#the interaction between total leeches and sample day was not significant across all sample days
# So, sampling day is a significant effect on baseline hematocrit, sampling day at hatching has a significant effect on baseline hematocrit.
#Looking at the effects plot, there does appear to be a similar trend where the number of leeches is negatively correlated to baseline hematocrit
#However, the plot also shows a lot of variability at emergence, and very little at oviposition. Also, my plots look like there is still a concentration of zeros on the X axis, perhaps a zero inflated model might be a better distribution. 


#3)Plot model using predict and overlay the model on top of underlying data. (use "type= response)

dat.new=expand.grid(Leeches_Total=seq(from = min(bloodclean$Leeches_Total),to = max(bloodclean$Leeches_Total),length.out = 174),
                    sampling.day = unique(bloodclean$sampling.day))

#using predict to plot points

#kept getting error so I removed the NAs from the dataframe in the data clean and reran the code, finally worked!!
dat.new$yhat  = predict(glm2,type="response",newdata = dat.new)
bloodclean$yhat2 = predict(glm2,type="response")
head(dat.new)


plot1=ggplot(data=bloodclean,aes(x=Leeches_Total,y=Mean_T0Hct,color=sampling.day))+
  geom_point(size=2,shape =1) + #plot points to show your data
  geom_line(data=dat.new, aes(x=Leeches_Total,y=yhat,col = sampling.day))
plot1

#based on the plot, there are a lot of zeros and smaller numbers that are clumping the data distribution. I might try this model again when learning zero inflated models

#4) write results statement like scientific paper.
# see README



##Week 11## Model comparisons

#1. Likelihood ratio tests and one other model selection approach to test 3 models (LM or GLMS)

#GLM models influencing baseline hematocrit

m1 = glm(Mean_T0Hct~1,data = bloodclean) #null model
m2 = glm(Mean_T0Hct~sampling.day,data = bloodclean)
m3 = glm.nb(Mean_T0Hct~sampling.day+Leeches_Total,data = bloodclean)# in the previous assignment, I found that leeches did not depend on sampling day so I will use it as an additive variable in this model
m4 = glm.nb(Mean_T0Hct~sampling.day+Leeches_Total+wounds,data = bloodclean) #wasn't sure what distribution to use because it has binary data and count data with many zeros?

summary(m2) #sample day had a significant effect on baseline hct
summary(m3) #showed that sampling day and leeches had an effect on hct
summary(m4) #showed that sampling day and leeches still were significant but open wounds was not significant to the model 
anova(m1,m2,m3,m4, test="LRT") #shows model 2 and 3 are significant

library(AICcmodavg)

AIC(m1,m2,m3,m4)

aictab(cand.set=list(m1,m2,m3,m4),modnames=c("m1","m2","m3","m4"))#received error that it didn't like the mixture of model classes
#looking at weight and AICcs
n=nrow(bloodclean)
tabA = AIC(m1,m2,m3,m4)
tabA$k<-c(m1$rank,m2$rank,m3$rank,m4$rank)
tabA$aiccs<-tabA$AIC+((2*tabA$k*(tabA$k+1))/(n-tabA$k-1))
#now order from smallest to biggest
tabA=tabA[order(tabA$aiccs),]
#calculate delta AIC
tabA$dAIC = tabA$aiccs - min(tabA$aiccs)
#you use the next two lines to get weights
tabA$edel<-exp(-0.5*tabA$dAIC) 
tabA$wt<-tabA$edel/sum(tabA$edel)
tabA #m3 is ranked highest but the delta AICs between m3,2,4 were all less than 2
Anova(m3) #shows both significant in the model

#2. Explain Results
#LRT: Of my models, I found that both number of leeches and sampling day had a significant effect on baseline hematocrit, open wounds did not significantly effect hematocrit. Based on LRT, I would select m3 for the best model
#AIC: m3 was ranked the best model using AIC. however, the delta AICs were all less than 1 apart from each other aside from the null.  

#3. synthesis statement on how the output of each approach is similar of different in the code

#While both approaches of model comparison ultimately lead me to the same result, the LRT was more informative about differences between models in this circumstance. 
#AIC results were too similar for definitive model selection.

