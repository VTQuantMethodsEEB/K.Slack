
#read cleaned up data into R

blooddat<-read.csv("BloodMetrics_20_21.csv")

##Note to Kate## I think its in tidy format?? but I was a bit confused on long vs wide and the internet wasn't very helpful

#KL: It's actually in wide format (each blood metric is a column and a row is a salamander)
# depending on what you want to do, you might want a column for 'blood_metric"
# and another column for timepoint with salamanders repeated

#load packages
library(tidyverse)
library(tidyr)
library(dplyr)

#examine data

#checking column names are correct and don't have any issues
head(blooddat)
#column names look okay

#checking my site names for inconsistencies
unique(blooddat$Site)
#all four sites match up

#checking the structure of my data frame to see how R classified my data
str(blooddat)
#Found out that R classified decimal numbers as "num" while it classified whole numbers as "int"
###Note to Kate## if an integer is a number then why is there a separate structure for it?? 
#And will some code not work if its an integer??

##KL: should be fine! Any integer, number, decimal, ultimately gets treated as a number
#checking distribution of metrics
hist(blooddat$TL) #looks like a normal distribution
hist(blooddat$Mass) #oooooop we found a mistake, histogram shows a value of zero
#replaced the zeros in the column with NA
blooddat$Mass[blooddat$Mass == 0]<-NA 
hist(blooddat$Mass) # fixed it--this histogram looks like a normal distribution


#play with "group by"

#I want to look at how my blood metrics differ across the different study sites but I have duplicate metrics
#make a new columns with the means of t0 and t60 hematocrit and hemoglobin

#new columns for T0 and T60 hemoglobin
blooddat$T0HbX=(blooddat$T0_HB1+blooddat$T0_HB2)/2
blooddat$T60HbX=(blooddat$T60_HB1+blooddat$T60_HB2)/2

#new columns for T0 and T60 hematocrit
blooddat$T0HctX=(blooddat$T0_Hct1+blooddat$T60_Hct2)/2
blooddat$T60HctX=(blooddat$T60_Hct1+blooddat$T60_Hct2)/2

#now I can look at the averages of the blood metrics across the different sites
#T0 hemoglobin
blooddat %>% 
  group_by(Site) %>% 
  summarise(T0.Hb=mean(T0HbX,na.rm=TRUE))

#T60 hemoglobin
blooddat %>% 
  group_by(Site) %>% 
  summarise(T60.Hb=mean(T60HbX,na.rm=TRUE))

#T0 hematocrit
blooddat %>% 
  group_by(Site) %>% 
  summarise(T0.Hct=mean(T0HctX,na.rm=TRUE))

#T60 hematocrit
blooddat %>% 
  group_by(Site) %>% 
summarise(T60.Hct=mean(T60HctX,na.rm=TRUE))

#I also wanted to see the average number leeches and leech bites
#leeches
blooddat %>% 
  group_by(Site) %>% 
  summarise(Leeches=mean(Leeches_Total,na.rm=TRUE))

#leech bites
blooddat %>% 
  group_by(Site) %>% 
  summarise(bite=mean(Leech.bites,na.rm=TRUE))

#I just used summarise to create tables in the output of site averages of blood metrics and leeches 
#Time to play with mutate--but I don't want to alter my blooddat so I am going to first create a new data frame
blooddat_mutate=blooddat %>% #new data frame
  group_by(Site,PIT.LAST.6)%>% #grouping the individual ID and site
  mutate(sample.size=n()) #added a column showing how many times the same individual was sampled across the season

##Mutate differs from summarise by altering the existing data frame and making a new column based on the specifications
##summarise doesn't alter the existing data frame, it creates summary tables in the output which can be used to create a new data frame


#KL: If you wanted to make the dataframe longer, you could use code from pivot_longer
#possibly something like this
blood.long = blooddat %>%
  select(-TL) %>%
  pivot_longer(    
    cols = starts_with(c("T")), #take all the cols that start with 'T'
    names_to = "blood_variable",
    values_to = "blood_value"
    
)

View(blood.long)
