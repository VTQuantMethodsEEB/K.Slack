#Assignment 1 Quantitative Methods
#1. Acquire data set --Hellbender blood metrics 2020

#2. reading data into R

blooddat<- read.csv("C:/Users/Katey Slack/Desktop/class stuff/QuantMethods/Slack_QuantitativeMethods/BloodMetrics_20_21.csv")
#i don't have this directory on my computer, should be:
blooddat<- read.csv("BloodMetrics_20_21.csv")

View(blooddat)

library(tidyverse)
library(tidyr)
library(dplyr)

#selecting column 6
blooddat[6]
#selecting row 12 in column 6
blooddat[12,6]

#selecting values in column
blooddat[blooddat$Site=="Thomas Bridge",]

#summary
summary(blooddat)

#3. calculations
#adding a column of for the log transformed total number of leeches 
#because I have many zeros in my leech column, I added a constant of 0.5
blooddat$logleech= log(blooddat$Leeches_Total+0.5)
hist(blooddat$logleech)


#4. Aggregate data

F1<- aggregate(logleech~Site+Date+Leech.bites,FUN=mean,data = blooddat)
  
#table
tab1<- table(blooddat$Site,blooddat$Leeches_Total)
print(tab1)


#testing git code changes
