# K.Slack

#Data Description

#For this course, I plan to use the data I have collected for my master’s thesis with the ultimate goal of being able to complete most of my data analysis by the end of the semester. I am researching pathogenic, behavioral, and environmental influences on red blood cell physiology of Eastern Hellbenders. Data was obtained from reproductive male hellbenders across their prolonged parental care period and repeated metrics taken at oviposition (day 0), mid-incubation (day 30), hatching (day 60), and emergence (day 200). In the data set, hellbenders’ PIT tag ID is listed, as well as the date and site of capture. At each sampling, a blood sample is taken immediately upon capture to establish baseline blood metrics of hematocrit (the proportion of red blood cells in circulation) and hemoglobin (the oxygen carrying protein found inside red blood cells). The baseline blood metrics are referred to as T0Hct and T0Hb in the data set and duplicates of each metric are taken for accuracy. Standard bodily metrics are recorded such as mass, snout-to-vent length, and total length. Presence of physical abnormalities and open wounds are documented, as well as the number of leeches and leech bites found on the individual. After 60 minutes, another blood sample was taken to evaluate the acute stress response. The post-stress blood metrics are referred to as T60Hct and T60Hb in the data set.

##week 1## Intro to R

#code:KSlack_assignment1.R

#data:BloodMetrics_20_21.csv

#I learned how to read a file into R and do some basic calculations to familiarize myself with the software such as making a table and aggregating the data.

##week 2## Tidy and Git

#code:Kslack_assignment2.R

#data:BloodMetrics_20_21.csv

#I used script to clean up the errors in my data. I used the Tidyvers and dplyr packages for grouping, summary tables, and mutating my data. 


##Week 3##

#code: week3_KateySlack.R

#data:BloodMetrics_20_21.csv

#I used ggplot to look at the distribution of hellbenders harboring leeches and the number of leech bites across my study sites using points and box plots, with my sites on the x axis and leeches/leechbites on the y axis. Even afer I transformed the data, the plots were still pretty difficult to read. I then looked at how the number of leeches and leech bites varied across the parental care period using date as the x axis, leeches/leechbites on the y axis, and colored the points by site. The plots were much easier to interpret and I could tell that hellbenders at the "762" study site had more leeches and leech bites compared to the other study sites.


##Week 4## 

##class disscussion of p-values and its original intents.


##Week 5##

#code:week5_KateySlack.R

#data:20_21Waterandblood.csv

#Temperature and dissolved oxygen concentrations in water are inversely related. As temperater decreases across season, hellbender metabolic activity decreases and environmental dissolved oxygen increases. Hellbenders will have less metabloic demands and greater oxygen supply, thus will have less red blood cells in circulation and more in splenic storage. 
##Hypothesis 1:Hellbender hematocrit and hemoglobin will be negatively correlated to Dissolved Oxygen concentrations. 


#Hellbenders engaging in parental behavior will have greater metabolic demands than hellbender not actively maintaining nests. Since there is difference in metabolic demands, there will be a difference in blood metrics.
##Hypothesis 2: Blood metrics differ between males exibiting parental care and males not maintaining nests


#I used pearsons correlation to test my first hypothesis and confirmed my hypothesis that there is a highly significant negative correlation between baseline hematocrit and hemoglobin and environmental dissolved oxygen concentrations. For my second hypthesis, I looked at the different distributions of various blood metrics using ggplot and decided to write a permutation for the blood metric that showed the biggest difference between nesters and nonnesters. I got the p value 0 because there were no residiuals greater than the observed mean. I ran a permutation using the coin package as well as a few different t-test and got a p-value around 0.05 for almost all the tests.







