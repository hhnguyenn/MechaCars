library(tidyverse)
library(jsonlite)

#load MPG Regression data
mpg <- read.csv('MechaCar_mpg.csv', check.names = F, stringsAsFactors=F)

#call dataset
head(mpg)

#multiple linear regression model
model <- (lm(mpg$"mpg" ~ mpg$"vehicle length" + mpg$"vehicle weight" + mpg$"spoiler angle" + mpg$"ground clearance" + mpg$"AWD"))
summary(model)

#Summary Statistic table for suspension coil
#load suspension coil data
suspension <- read.csv ('Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
#group by manufacturing lot and find median, mean, SD, and variance
suspension_summary <- suspension %>% 
  group_by(Manufacturing_Lot) %>% 
  summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),SD_PSI=sd(PSI), Var_PSI=var(PSI)) 
 
#Suspension Coil T-test
#compare suspension coil's to population mean of 1500 pounds per inch

# Lot 3
Lot3_PSI <- suspension %>% 
  filter(Manufacturing_Lot=="Lot3") 
#t.test(log10(Lot3_PSI$PSI), mu=mean(log10(suspension$PSI)))
t.test(log10(Lot3_PSI$PSI), mu=mean(log10(1500)))

# Lot 2
Lot2_PSI <- suspension %>% 
  filter(Manufacturing_Lot=="Lot2")
t.test(log10(Lot2_PSI$PSI), mu=mean(log10(1500)))

#Lot 1
Lot1_PSI <- suspension %>% 
  filter(Manufacturing_Lot=="Lot1")
t.test(log10(Lot1_PSI$PSI), mu=mean(log10(1500)))
