#load in packages

library(ggplot)
library(ggplot2)
library(gridExtra)
library(viridis)

#reading my blood metrics into R and naming the data frame "blooddat"
blooddat=read.csv("BloodMetrics_20_21.csv")
head(blooddat)

#I want to make a graph showing the distribution of leeches at each site and name it "leechplot"
leechplot=ggplot(data=blooddat, aes(x = Site, y = Leeches_Total))+
  geom_point(size=2) #my plot is not super informative because of all the zeros

#making a box plot with the number of leech bites at each plot and name it "leechbite"
leechbite=ggplot(data=blooddat, aes(x = Site, y = Leech.bites))+
  geom_boxplot()
leechbite
#it looks like it could win the ugliest ggplot contest-- I am going to change it to points
leechbite=ggplot(data=blooddat, aes(x = Site, y = Leech.bites))+
  geom_point(size=2)
leechbite
#since my data has many zeros, neither box plot nor point graph looks good. I am going to transform my data then try again
#adding a column to log transform leeches and leech bites 
blooddat$logbite=log(blooddat$Leech.bites+0.5)
blooddat$logleech=log(blooddat$Leeches_Total+0.5)

#new plots with transformed data
logleechplot=ggplot(data=blooddat, aes(x = Site, y = logleech))+
  geom_point(size=2)
logleechplot #visually looks like a much better distribution, lets see if its an okay box plot

logleechbox=ggplot(data=blooddat, aes(x = Site, y = logleech))+
  geom_boxplot()
logleechbox #better than the first but still not too pretty and I definitely could not interpret the data just looking at it

#I am moving on and going to use different data 
#I want to see the distribution of leeches and leech bites across the hellbender parental care period and color it by site

#I found a mistake in my data where one of the dates is in excel format so I replaced it with the correct date
blooddat$Date[blooddat$Date=="44109"]<-"10/05/2020"

#telling R my Date is a date
blooddat$date.new=as.Date(blooddat$Date, "%m/%d/%Y")
#Could not figure out why my 2021 dates were saved as 2020
##45 minutes and several google searches later, I found out the importance of a capital Y

#plotting new date with log leech bites
logbiteplot=ggplot(data=blooddat,aes(x=date.new,y=logbite,color=Site))+
  geom_point(size=2)
logbiteplot
##PROGRESS! this looks okay but I want to make the dates look better

logbiteplot2=ggplot(data=blooddat,aes(x=date.new,y=logbite,color=Site))+
  geom_point(size=2)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y", limits =as.Date(c('2020-9-6','2021-4-20')))
logbiteplot2 #SUCCESS!
##I can see from this plot that hellbenders at 762_Bridge had more leech bites across season compared to other sites

##now I am going to look and log leeches
logleechdate=ggplot(data=blooddat,aes(x=date.new,y=logleech,color=Site))+
  geom_point(size=2)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y", limits =as.Date(c('2020-9-6','2021-4-20')))
logleechdate

#This looks pretty nice!
##I can see from my data that hellbenders at 762_Bridge site harbored more leeches across season than the other sites
