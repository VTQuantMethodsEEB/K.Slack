
### Kate, I am so sorry that this is late coming to you, and for its ambitious length.
#I really wanted to take advantage of your feedback, so I attempted to incorporate as many of my research questions into this project as possible. 

## Quant Methods Final Project ##

#Hypothesis 1: Hemoconcentration in response to stress

#Hypothesis 2: Effects of seasonal variation and body size (i.e., total length in cm) 

#Hypothesis 3: Influence of parasites (leeches and trypanosomes), open wounds/immune repair, and polychromasia

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
library(emmeans)
library(AICcmodavg)
# read in data
blood= read.csv("fullblood2020.csv")
blood20=dplyr::select(blood, -c(8, 9, 11, 12, 15, 16, 18, 19, 21, 22, 25, 26, 29, 30, 31, 33, 34, 35, 37, 68, 71 ))

##Hypothesis 1: Will hematocrit and hemoglobin change in response to acute stress?
  
#altering layout of data to show time as treatment
Hct = gather(blood20, key = time, value = Hct, c(11:12))
Hb=gather(blood20, key = time, value = Hb, c(8:9))

#removing nas
Hct=Hct[complete.cases(Hct), ]
Hb= Hb[complete.cases(Hb), ]

#spearmans correlation test
cor.test(blood20$T0HB, blood20$T0Hct, method = 'spearman')

#Creating null models
nullhct= lmer(Hct~1 + (1|PIT), data = Hct)
nullhb= lmer(Hb~1 + (1|PIT), data =Hb)

#mixed effects model with blood as response, time as fixed effects (treatment), PIT tag as random effects

#stress and hematocrit
 H1.hct <- lmer(Hct~time + (1|PIT),data=Hct)
 summary(H1.hct)  
 Anova(H1.hct)#time has significant effect on Hematocrit
 anova(H1.hct,nullhct) #model is better than the null
 #stress and Hemoglobin
 H1.hb <- lmer(Hb~time + (1|PIT),data=Hb)
summary(H1.hb)  
Anova(H1.hb)# time has significant effects on Hemoglobin
anova(H1.hb,nullhb)

#results: Time exposed to acute stressor effects hematocrit and hemoglobin
##############################################

###Hypothesis 2: Does seasonal variation and body size influence blood metrics and hemoconcentration response?

#relevel sample day
Hct$Sampling.day=factor(Hct$Sampling.day, c("Oviposition", "Midincubation", "Hatching", "Emergence"))
unique(Hct$Sampling.day)

Hb$Sampling.day=factor(Hb$Sampling.day, c("Oviposition", "Midincubation", "Hatching", "Emergence"))
unique(Hb$Sampling.day)

###season and hematoctrit
H2.1Hct <- lmer(Hct~time+Sampling.day + (1|PIT),data=Hct)
summary(H2.1Hct)
plot(allEffects(H2.1Hct))
Anova(H2.1Hct)

#effects of stress on blood depend on sampling day 
H2.2Hct <- lmer(Hct~time*Sampling.day + (1|PIT),data=Hct)
summary(H2.2Hct)
Anova(H2.2Hct)
plot(allEffects(H2.2Hct)) #time, day, and interaction were significant in model

#effects of stress on blood depend on temperature 
H2.3Hct <- lmer(Hct~time*X24hrMedC + (1|PIT),data=Hct)
summary(H2.3Hct)
Anova(H2.3Hct) # time and temp have significant effects but not the interaction term
plot(allEffects(H2.3Hct))

#effects of stress in addition to temperature
H2.4Hct <- lmer(Hct~time+X24hrMedC + (1|PIT),data=Hct)
summary(H2.4Hct)
Anova(H2.4Hct)
#model comparisons
anova(H2.1Hct, H2.2Hct, H2.3Hct, H2.4Hct, nullhct)
#LRT ranked H2.2 as best model

####Season and hemoglobin
##repeating the same code but for hemoglobin

H2.1Hb <- lmer(Hb~time+Sampling.day + (1|PIT),data=Hb)
summary(H2.1Hb) #significant differences in sampling day
Anova(H2.1Hb)#time and sampling day has significant effects on hemoglobin
plot(allEffects(H2.1Hb))

H2.2Hb <- lmer(Hb~time*Sampling.day + (1|PIT),data=Hb)
summary(H2.2Hb) #significant differences across sample day and time, not interaction
Anova(H2.2Hb)
plot(allEffects(H2.2Hb))

H2.3Hb <- lmer(Hb~time*X24hrMedC + (1|PIT),data=Hb)
summary(H2.3Hb) #significant but not interaction
Anova(H2.3Hb)
plot(allEffects(H2.3Hb))

H2.4Hb <- lmer(Hb~time+X24hrMedC + (1|PIT),data=Hb)
summary(H2.4Hb)
#comparisons
anova(H1.hb,H2.1Hb, H2.2Hb, H2.3Hb, H2.4Hb,nullhb)
#LRT ranked H2.2 as best model, but there was a small difference in AIC (2.61) between the additive and interaction models and the models did not significantly differ from each other

aictab(cand.set=list(nullhb, H1.hb,H2.1Hb, H2.2Hb, H2.3Hb, H2.4Hb),modnames=c("nullhb","H1.Hb","H2.1Hb", "H2.2Hb", "H2.3Hb", "H2.4Hb"))
aictab(cand.set=list(nullhct, H1.hct,H2.1Hct,H2.2Hct,H2.3Hct, H2.4Hct),modnames=c("nullhct","H1.hct","H2.1Hct","H2.2Hct","H2.3Hct", "H2.4Hct"))

#based on the results from LRT, AICc rank, and biological hypotheses, I will use the time*sample day interaction in my further models
coef(summary(H2.2Hb))
coef(summary(H2.2Hct))

#### accounting for body size
H2.5Hct <-  lmer(Hct~TL.cm.+ (1|PIT),data=Hct)
summary(H2.5Hct)
plot(allEffects(H2.5Hct)) #no strong significant effect on hematocrit (o.58)
Anova(H2.5Hct)

H2.5Hb <- lmer(Hb~TL.cm.+ (1|PIT),data=Hb)
summary(H2.5Hb)
Anova(H2.5Hb) #no significant effects on hemoglobin

H2.6Hct <- lmer(Hct~time+TL.cm.+ (1|PIT),data=Hct)
summary(H2.6Hct)
Anova(H2.6Hct) #total length and time both significant effect on hematocrit 

H2.6Hb<- lmer(Hb~time +TL.cm.+ (1|PIT),data=Hb)
summary(H2.6Hb)
Anova(H2.6Hb) #time is significant, not TL

H2.7Hct <-lmer(Hct~time*Sampling.day + TL.cm. + (1|PIT),data=Hct)
summary(H2.7Hct)
Anova(H2.7Hct) #time, sampling day, and interaction all significant, TL not significant

H2.7Hb<- lmer(Hb~time*Sampling.day + TL.cm.+ (1|PIT),data=Hb)
summary(H2.7Hb)
Anova(H2.7Hb)#time and sampling day is significant, TL and interaction are not significant

anova(H2.5Hct,H2.6Hct,H2.7Hct)
anova(H2.5Hb, H2.6Hb,H2.7Hb)
#LRT ranked H2.7Hct and H2.7Hb as best models 

#######predicting and plotting seasonal effects#######
newdat= expand.grid(time = unique(Hct$time),
                    PIT = unique(Hct$PIT),
                    X24hrMedC = seq(min(Hct$X24hrMedC),max(Hct$X24hrMedC),by = .05))

newdat$yhat = predict(H2.3Hct,newdata= newdat,re.form=NA,type="response")

fig=ggplot(data=newdat, aes(x=X24hrMedC,y=yhat,col=time))+ 
  geom_line(size=1)+
  geom_point(data = Hct, aes(x = X24hrMedC, y = Hct, fill=time),size=3, shape = 21)+
  ylab("Hematocrit %")+
  xlab("Temperature Celsius")+
  theme_bw() + 
  theme(axis.title=element_text(size=18),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c("right"),legend.text = element_text(size=12,face="italic"))
print(fig)

newdat2 = expand.grid(time = unique(Hct$time),
                     PIT = unique(Hct$PIT),
                     Sampling.day = unique(Hct$Sampling.day))
newdat2$yhat = predict(H2.2Hct,newdata= newdat2,re.form=NA,type="response")

Fig2=ggplot(data = Hct, aes(x=Sampling.day , y = Hct, fill= time)) +
  geom_boxplot(alpha=0.9)+
  geom_point(aes(fill = time), size = 3, shape = 21, position = position_jitterdodge()) +
  geom_line(data=fortify(newdat2), aes(x=Sampling.day,y=yhat), size= 2, position = position_jitterdodge())+
  ylab("Hematocrit %")+
  xlab("Sample Day")+
  theme_bw() + 
  theme(axis.title=element_text(size=18),axis.text=element_text(size=12),panel.grid = element_blank(), axis.line=element_line(),legend.position="right",legend.text = element_text(size=12,face="italic"))
print(Fig2)
#####################################################################

###Hypothesis 3.Influence of parasites (leeches and trypanosomes), open wounds/immune repair, and polychromasia

##parasite prevalence in study system
table(blood20$Tryps_TotalBC>0)
table(blood20$Leeches_Total>0)
table(blood20$Leeches_Total>0, blood20$Tryps_TotalBC>0) #all animals with leeches were infected with trypanosomes

##difference in parasite presence across sampling day
p1=glmer(Leeches_Total~Sampling.day + (1|PIT), family = "poisson", data=Hct)
Anova(p1)
summary(p1)
plot(allEffects(p1)) 
## Checking for over dispersion 
var(Hct$Leeches_Total)#24.675
mean(Hct$Leeches_Total)#1.18
#definitely over dispersed, fitting the same model with negative binomial to account for zeros
p1nb=glmer.nb(Leeches_Total~Sampling.day + (1|PIT),data=Hct) # I got a weird error but it ran?
summary(p1nb) 
Anova(p1nb)
AIC(p1,p1nb) # P1 is slightly better

p2= glmer(Tryps_TotalBC~Sampling.day + (1|PIT), family = "poisson", data=Hct)
Anova(p2)
summary(p2)
plot(allEffects(p2))
exp(coef(summary(p1)))
exp(coef(summary(p2)))

##looking at White blood cell differentials to determine if open wounds and parasitic presence provoke immune response 
#### proportion of polychromasia per 50 FOVs
Hct$EstRBC.50= as.numeric(Hct$EstRBC.50)
Hct$polyhct= ((Hct$Polychromasia/Hct$EstRBC.50))
hist(Hct$polyhct) #not normally distributed, so transform data

Hb$EstRBC.50= as.numeric(Hb$EstRBC.50)
Hb$polyhb= ((Hb$Polychromasia/Hb$EstRBC.50))
#removing nas
Hct=Hct[complete.cases(Hct), ]
Hb= Hb[complete.cases(Hb), ]
####evaluate if open wounds influence White blood cell profiles ####

#new cols for immature neutrophils
Hct$band = (Hct$Prop.IN+Hct$Prop.ITN)
Hct$bandratio = (Hct$band/Hct$Prop.AllNeut)
Hb$band = (Hb$Prop.IN+Hb$Prop.ITN)
Hb$bandratio = (Hb$band/Hb$Prop.AllNeut)

#new cols for toxic neutrophils
Hct$toxic = (Hct$Prop.TMN+Hct$Prop.ITN)
hist(log(Hct$N.Lratio))
hist(log(Hct$IMNratio)) #both transformed into normal distribution

H3.1<- lmer(log(N.Lratio)~Wounds + (1|PIT), data=Hct)#Neutrophil:lymphocyte ratio
H3.2<- lmer(log(TNratio)~Wounds + (1|PIT),data=Hct)
H3.3<- lmer(IMNratio~Wounds + (1|PIT),data=Hct)
H3.4<- lmer(log(polyhct)~Wounds + (1|PIT),data=Hct)
H3.5<- lmer(band~Wounds+ (1|PIT),data=Hct)
H3.6<- lmer(bandratio~Wounds+ (1|PIT),data=Hct)

summary(H3.1) #wounds not a significant predictor
Anova(H3.1)
summary(H3.2)
Anova(H3.2) #wounds not a significant predictor for Toxic mature neutrophils
summary(H3.3)
Anova(H3.3)#wounds not a significant predictor for immature not toxic neutrophils
summary(H3.4)
Anova(H3.4) #wounds not significant predictor for %poly
summary(H3.5)
Anova(H3.5)#wounds not a significant predictor for the total proportion of immature neutrophils, but toxic and non toxic
summary(H3.6)
Anova(H3.6)#wounds not a significant predictor for the ratio of immature:mature neutrophils

### evaluate if parasites influence WBCs

#scaling tryp and leech because my models were not liking the fit
Hct$tryp=scale(Hct$Tryps_TotalBC, center= TRUE, scale= TRUE )
Hct$leech=scale(Hct$Leeches_Total, center= TRUE, scale= TRUE )
Hb$tryp=scale(Hb$Tryps_TotalBC, center= TRUE, scale= TRUE )
Hb$leech=scale(Hb$Leeches_Total, center= TRUE, scale= TRUE )

Hp.1<- lmer(log(N.Lratio)~leech*tryp + (1|PIT), data=Hct)#leeches and interaction  significant N:L ratios increase with both
Hp.2<- lmer(TNratio~leech*tryp + (1|PIT),data=Hct)# no effects on toxic Neut/total ratio
Hp.3<- lmer(Prop.TMN~leech*tryp + (1|PIT),data=Hct)# Leeches increase and interaction decreases significant
Hp.4<- lmer(Prop.eosin~leech*tryp + (1|PIT),data=Hct) #no effects
Hp.5<- lmer(Prop..Baso~leech*tryp + (1|PIT), data=Hct) #leeches increase, interaction decreases, both significant, interaction more so
Hp.6<- lmer(IMNratio~leech*tryp + (1|PIT), data=Hct)#no effects 
Hp.7<- lmer(B.Eratio~leech*tryp + (1|PIT), data=Hct)#significant effect on interaction, Basophil:Eosinophil ratio decrease with the interaction
Hp.8<- lmer(Prop.IN~leech*tryp + (1|PIT), data=Hct)#no effects 
Hp.9<- lmer(band~leech*tryp + (1|PIT), data=Hct)#no effects 
Hp.10<- lmer(bandratio~leech*tryp + (1|PIT), data=Hct)#no effects 
Hp.11<- lmer(toxic~leech*tryp + (1|PIT), data=Hct)#leeches significantly increased proportion of toxic neutrophils and interaction significantly decreased toxic neutrophils

summary(Hp.1)
Anova(Hp.1)
exp(coef(summary(Hp.1)))

summary(Hp.2)
Anova(Hp.2)

summary(Hp.3)
Anova(Hp.3)
(coef(summary(Hp.3)))

summary(Hp.4)
Anova(Hp.4) 

summary(Hp.5)
Anova(Hp.5)
(coef(summary(Hp.5)))

summary(Hp.6)
Anova(Hp.6)

summary(Hp.7)
Anova(Hp.7)

summary(Hp.8)
Anova(Hp.8)

summary(Hp.9)
Anova(Hp.9)
summary(Hp.10)
summary(Hp.11)
Anova(Hp.11)
(coef(summary(Hp.11)))
### I know this is a lot. But for my thesis, I will probably use a multivariate approach for my WBCs

##determine if parasites affect polychromasia
pt=lmer(log(polyhct)~tryp + (1|PIT), data=Hct)
summary(pt)
Anova(pt)
exp(coef(summary(pt))) #tryps significantly increases poly

ptl=lmer(log(polyhct)~tryp*leech + (1|PIT), data=Hct)
summary(ptl)
Anova(ptl) #Tryps significant but leeches and interaction are not
exp(coef(summary(ptl)))

ptlb=lmer(log(polyhct)~tryp*leech + Leech.bites+ (1|PIT), data=Hct)
summary(ptlb)
Anova(ptlb) 
exp(coef(summary(ptlb)))# all but interaction was significant

##effects of poly on blood
#log transformed poly for better distribution
polHct<- lmer(Hct~time+log(polyhct) + (1|PIT), data=Hct)
Anova(polHct) #significant effects of polychromasia on blood
summary(polHct)
plot(allEffects(polHct))

polHb<- lmer(Hb~time+log(polyhb) + (1|PIT), data=Hb)
Anova(polHb) #significant effects of polychromasia on blood
summary(polHb)


#Parasites on blood##
P1Hct<- lmer(Hct~time+leech + (1|PIT), data=Hct)
Anova(P1Hct) #time was significant and leech was not

P2Hct<- lmer(Hct~time+Leech.bites+leech + (1|PIT), data=Hct)
Anova(P2Hct) #leech bites is approaching significance but not strong, 0.06

P3Hct<- lmer(Hct~time+leech+tryp + (1|PIT), data=Hct)
Anova(P3Hct) #only time effects Hct

P4Hct<- lmer(Hct~time*tryp + (1|PIT), data=Hct)
Anova(P4Hct) #time is significant and tryp is approaching significance but not strong, 0.07

P1Hb<-lmer(Hb~time+leech + (1|PIT), data=Hb)
Anova(P1Hb) #time significant but not leech

P2Hb<- lmer(Hb~time*leech+Leech.bites + (1|PIT), data=Hb)
Anova(P2Hb) 
summary(P2Hb)#interaction was not significant, but leeches significantly increased T0hb, and leech bites decrease T0Hb but I think its correlated with season

P3Hb<- lmer(Hb~time+leech+tryp+ (1|PIT), data=Hb)
Anova(P3Hb)
summary(P3Hb) #tryps significantly decrease T0hb

P4Hb<- lmer(Hb~time*tryp + (1|PIT), data=Hb)
Anova(P4Hb) 
summary(P4Hb) #trypanosomes significantly decreased hemoglobin but did not signifcantly effect T60

##parasites significantly effect hemoglobin, potentially hematocrit but not clear

#################################################################################
Hct=Hct[complete.cases(Hct), ]
Hb= Hb[complete.cases(Hb), ]
#####FULL MODEL COMPARISONS##########

nullhct= lmer(Hct~1 + (1|PIT), data = Hct)
nullhb= lmer(Hb~1 + (1|PIT), data =Hb)
##Hypothesis 1: Stress induced hemoconcentration

#stress and hematocrit
nullhct
k1 <- lmer(Hct~time + (1|PIT),data=Hct)
k2  <- lmer(Hct~time + TL.cm.+(1|PIT),data=Hct) ##significantly better
anova(nullhct, k1,k2)
#stress and Hemoglobin
nullhb
s1<- lmer(Hb~time + (1|PIT),data=Hb) ##preferred
s2<- lmer(Hb~time +TL.cm.+(1|PIT),data=Hb) #did not significantly improve from s1
anova(nullhb,s1,s2)


#Hypothesis 2: Effects of seasonal variation and body size (i.e., total length in cm) 

##seasonal effects and body size on Hematocrit 
k3 <-lmer(Hct~time*Sampling.day + TL.cm.+(1|PIT),data=Hct) #highest rank model
k4 <-lmer(Hct~time*X24hrMedC + TL.cm.+ (1|PIT),data=Hct)
anova(nullhct,k1,k2,k3,k4)
####Season and body size influence hemoglobin
s3 <-lmer(Hb~time*Sampling.day + TL.cm.+(1|PIT),data=Hb) #highest rank model
s4 <-lmer(Hb~time*X24hrMedC + TL.cm.+ (1|PIT),data=Hb)
anova(nullhb,s1,s2,s3,s4)

#Hypothesis 3: Influence of parasites (leeches and trypanosomes), open wounds/immune repair, and polychromasia

#Parasites on hematocrit##

k5<- lmer(Hct~time*leech + (1|PIT), data=Hct)
k6<- lmer(Hct~time*Sampling.day+Leech.bites+tryp + (1|PIT), data=Hct) #k3 still best, but k6 and k7 were similar but not significantly better
k7<- lmer(Hct~time*Sampling.day+leech+tryp + (1|PIT), data=Hct)
k8<- lmer(Hct~time*tryp + (1|PIT), data=Hct)
anova(nullhct,k1,k2,k3,k4,k5,k6,k7,k8)

#parasites on hemoglobin
s5<-lmer(Hb~time*leech + (1|PIT), data=Hb)
s6<- lmer(Hb~time*Sampling.day+Leech.bites+tryp + (1|PIT), data=Hb) #s3 still best, but s6 and s7 were similar but not improved
s7<- lmer(Hb~time*Sampling.day+leech+tryp+ (1|PIT), data=Hb)
s8<- lmer(Hb~time*tryp + (1|PIT), data=Hb)
anova(nullhb,s1,s2,s3,s4,s5,s6,s7,s8)

##effects of poly on blood
k9<- lmer(Hct~time+log(polyhct) + (1|PIT), data=Hct)
k10<-lmer(Hct~time*Sampling.day+log(polyhct) + (1|PIT), data=Hct)
k11<-lmer(Hct~time*Sampling.day+log(polyhct) + tryp+ Leech.bites + (1|PIT), data=Hct)
anova(nullhct,k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11) #k3 still best model, k10 k6 k7 and k11 were all similar but not significantly improved

s9<- lmer(Hb~time+log(polyhb) + (1|PIT), data=Hb)
s10<-lmer(Hb~time*Sampling.day+log(polyhb) + (1|PIT), data=Hb)
s11<-lmer(Hb~time*Sampling.day+log(polyhb)+ tryp*Leech.bites + (1|PIT), data=Hb)
anova(nullhb,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11)#s3 still best model, s10 s6 s7 s11 were all similar but not signfcantly better

##effects of open wounds and immune repair on blood##
k12<- lmer(Hct~time+Wounds + (1|PIT), data=Hct)
k13<-lmer(Hct~time*Sampling.day + Wounds + leech + (1|PIT), data=Hct)
k14<-lmer(Hct~time*Sampling.day + Wounds + tryp*Leech.bites + (1|PIT), data=Hct)
k15<-lmer(Hct~time+N.Lratio+toxic + (1|PIT), data=Hct)
anova(k1,k2,k3,k4,k5,k6,k7,k9,k10,k11,k12,k13,k14,k15,nullhct) #k3 still best, k10 k6 k7 k13 k11 k14 similar but not statistically improved
s12<- lmer(Hb~time+Wounds + (1|PIT), data=Hb)
s13<-lmer(Hb~time*Sampling.day+Wounds +leech + (1|PIT), data=Hb)
s14<-lmer(Hb~time*Sampling.day+Wounds+ tryp*Leech.bites + (1|PIT), data=Hb)
s15<-lmer(Hb~time*Sampling.day+Wounds+ tryp*Leech.bites + (1|PIT), data=Hb)
anova(nullhb,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15)#s3 still best model, s10 s6 s7 s11 were all similar but not signfcantly better



aictab(cand.set=list(nullhct, k1,k2,k3,k4,k5,k6,k7,k9,k10,k11,k12,k13,k14,k15),modnames=c("nullhct","k1","k2","k3","k4", "k5","k6","k7","k8", "k9", "k10", "k11", "k12", "k13", "k14"))

aictab(cand.set=list(nullhb,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15),modnames=c("nullhb","s1","s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12", "s13", "s14", "s15"))

#slimming down
aictab(cand.set=list(nullhb,s2,s3,s6,s7,s10,s13,s14),modnames=c("nullhb","s2", "s3","s6", "s7", "s10",  "s13", "s14"))

aictab(cand.set=list(nullhct,k3,k6,k7,k10,k12,k13),modnames=c("nullhct","k3","k6","k7", "k10", "k12", "k13"))
