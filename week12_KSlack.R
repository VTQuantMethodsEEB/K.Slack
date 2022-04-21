## Mixed effects models## week 12 

#1. run a mixed model on one or more hypothese (note if model does not run or if you violate assumptions)
##Hypothesis 1): I want to determine if there are seasonal differences in Hematocrit and Hemoglobin at baseline and post acute stress.  
## response:  hematocrit and hemoglobin
## Fixed effects: Sampling day / Temperature
## Random: PIT Tag

##load important packages##
# read in data
blood= read.csv("fullblood2020.csv")
blood20=dplyr::select(blood, -c(8, 9, 11, 12, 15, 16, 18, 19))

##load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)
library(effects)
library(emmeans)
library(AER)
library(car)
library(lme4)
#altering layout of data to show time as treatment

Hct = gather(blood20, key = time, value = Hct, c(11:12))
Hb=gather(blood20, key = time, value = Hb, c(8:9))

#spearmans correlation test
cor.test(Hb$Hb, Hct$Hct, method = 'spearman')
##Hypothesis 1: Will hematocrit and hemoglobin change in response to acute stress?

#mixed effects model with blood as response, time as fixed effects (treatment), PIT tag as random effects
#stress and hematocrit
H1.hct <- lmer(Hct~time + (1|PIT),data=Hct)
summary(H1.hct)  
Anova(H1.hct) #time has significant effect on Hematocrit
#stress and Hemoglobin
H1.hb <- lmer(Hb~time + (1|PIT),data=Hb)
summary(H1.hb)  
Anova(H1.hb)# time has significant effects on Hemoglobin
##results: Time exposed to acute stressor effects hematocrit and hemoglobin

##Hypothesis 2: Will the stress response be influenced by season?
#effects of stress in addition to sampling day
H2.1Hct <- lmer(Hct~time+Sampling.day + (1|PIT),data=Hct)
summary(H2.1Hct)
plot(allEffects(H2.1Hct))
Anova(H2.1Hct)
#effects of stress on blood depend on sampling day 
H2.2Hct <- lmer(Hct~time*Sampling.day + (1|PIT),data=Hct)
summary(H2.2Hct)
Anova(H2.2Hct)
plot(allEffects(H2.2Hct))
#effects of stress on blood depend on temperature 
H2.3Hct <- lmer(Hct~time+X24hrMedC + (1|PIT),data=Hct)
summary(H2.3Hct)
plot(allEffects(H2.3Hct))
Anova(H2.3Hct)
anova(H2.1Hct,H2.2Hct,H2.3Hct)

H2.1Hb <- lmer(Hb~time+Sampling.day + (1|PIT),data=Hb)
summary(H2.1Hb) #significant differences in sampling day, Mid incubation is only day that didn't significantly differ from emergence
Anova(H2.1Hb)#time and sampling day has significant effects on hemoglobin
plot(allEffects(H2.1Hb))

H2.2Hb <- lmer(Hb~time*Sampling.day + (1|PIT),data=Hb)
summary(H2.2Hb) 
plot(allEffects(H2.2Hb))

H2.3Hb <- lmer(Hb~time+X24hrMedC + (1|PIT),data=Hb)
summary(H2.3Hb)
plot(allEffects(H2.3Hb))
Anova(H2.3Hb)
#comparisons
anova(H1.hb,H2.1Hb, H2.2Hb, H2.3Hb)
library(AICcmodavg)
aictab(cand.set=list(H1.hb,H2.1Hb, H2.2Hb, H2.3Hb),modnames=c("H1.Hb","H2.1Hb", "H2.2Hb", "H2.3Hb"))
anova(H1.hct,H2.1Hct,H2.2Hct,H2.3Hct)
aictab(cand.set=list(H1.hct,H2.1Hct,H2.2Hct,H2.3Hct),modnames=c("H1.hct","H2.1Hct","H2.2Hct","H2.3Hct"))
## I got some weird errors back from my AICtab

#creating figures

Fig1=Hct %>%
  ggplot( aes(x=reorder(Sampling.day, desc(Sampling.day)) , y = Hct, fill=time)) +
  geom_boxplot(alpha = 0.90) +
  geom_point(aes(fill = time), size = 3, shape = 21, position = position_jitterdodge()) +
  ylab("Hematocrit %")+
  xlab("Sample Day")+
   theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position="right",legend.text = element_text(size=12,face="italic"))
print(Fig1)
  

fig2=Hct %>%
  ggplot( aes(x=X24hrMedC , y = Hct, color=time)) +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = time))+
  ylab("Hematocrit %")+
  xlab("Degrees Celsius")+
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position="right",legend.text = element_text(size=12,face="italic"))
print(fig2)

#2. Write results statement based on model output on separate document and submit to canvas (can include tables)
#some not all results

#3. update read me

