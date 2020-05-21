x <-3
install.packages("tidyverse")
demo_table<-read.csv(file='demo.csv', check.names=F, stringsAsFactors = F)
library(jsonlite)
demo_table2 <- fromJSON(txt='demo.json')

#Filter by price and drivetrain
filter_table2<-subset(demo_table2, price > 10000 & drive =="4wd" & "clean" %in% title_status)

#Sampling
demo_table[sample(1:nrow(demo_table),3),]

library(tidyverse)
?mutate()

#add columns to original data frame
demo_table<- demo_table %>% mutate(Mileage_per_Year = Total_Miles/(2020-Year), IsActive= TRUE)

#create summary table by group used car data by condition of vehicle and mileage per condition
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarise(Mean_Mileage = mean(odometer))

#create summary table with multiple columns
summarize_demo <- demo_table2 %>% group_by(condition)%>% summarize(Mean_Mileage=mean(odometer),Maximum_Price=max(price),Num_Vehicle= n())

#load demo2
demo_table3<- read.csv('demo2.csv', check.names = F, stringsAsFactors = F)

#use gather() to change dataset to a long format
long_table <- gather(demo_table3, key="Metric", value="Score",buying_price:popularity)

#use spread() to spread out long-format data frame
wide_table<- long_table%>% spread(key = "Metric",value="Score")

#Check to see if equal
all.equal(demo_table3, wide_table)

#call MPG dataset (built into R)
head(mpg)

#import dataset into ggplot2
plt<- ggplot(mpg, aes(x=class))

#plot a bar plot
plt+geom_bar()

#create a summary table
mpg_summary <- mpg%>%group_by(manufacturer) %>% summarize(Vehicle_Count=n())

#Import dataset into ggplot2
plt<- ggplot(mpg_summary, aes(x=manufacturer, y=Vehicle_Count))

#plot a bar plot
plt+geom_col()

#plot bar plot with labels
plt+geom_col() + xlab("Manufacturing Company")+ ylab("Number of Vehicles in Dataset")

#plot a boxplot with labels
plt+geom_col() + xlab("Manufacturing Company") + 
  ylab("Number of Vehicle in Dataset") +
  #rotate the x-axis label
  theme(axis.text.x=element_text(angle=45, hjust=1))

#create summary table
mpg_summary <- subset(mpg, manufacturer=="toyota") %>% group_by(cyl) %>% summarize (Mean_Hwy=mean(hwy))

#import dataset into ggplot2
plt<- ggplot(mpg_summary, aes(x=cyl, y=Mean_Hwy))
#plot line
plt+geom_line()
#add line plot with labels
plt+geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30))

#scatterplot
#import dataset into ggplot2
plt<- ggplot(mpg, aes(x=displ, y=cty))
#add scatter plot with labels
plt+geom_point() + xlab("Engine Size (L)") + ylab ("City Fuel-Efficiency (MPG")

#add color=class
plt<- ggplot(mpg, aes(x=displ, y=cty, color=class))
# add color
plt+geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class")

#import dataset into ggplot2
plt <- ggplot(mpg, aes(x=displ, y=cty, color=class, shape=drv))
#ad scatter plot with multiple aesthetics
plt + geom_point() + labs(x="Engine SIze (L)", y="City Fuel-Efficiency (MPG", color="Vehicle Class", shape="Type of Drive")

#boxplot
#import dataset into ggplot2
plt<- ggplot(mpg, aes(y=hwy))
#add boxplot
plt + geom_boxplot()

#create set of boxplots
#import dataset into ggplot2
plt<- ggplot(mpg,aes(x=manufacturer, y=hwy))
#add boxplot and rotate x-axis labels 45 degrees
plt+ geom_boxplot() + theme(axis.text.x=element_text(angle=45, hjust=1))

#heatmap
#create summary table
mpg_summary<- mpg %>% group_by(class, year) %>% summarize (Mean_Hwy=mean(hwy))
plt<- ggplot(mpg_summary, aes(x=class, y=factor(year), fill=Mean_Hwy))
#create heatmap with labels
plt +geom_tile() + labs(x="Vehicle Class", y="Vehicle Year", fill="Mean Highway (MPG)")

#create summary table
mpg_summary <- mpg %>% group_by(model, year) %>% summarize (Mean_Hwy=mean(hwy))
#import dataset into ggplot2
plt <- ggplot(mpg_summary, aes(x=model, y=factor(year), fill=Mean_Hwy))
#add heatmap with labels 
plt + geom_tile() + labs(x="Model", y="Vehicle Year", fill = "Mean Highway (MPG)") +
#rotate x-axis labels 90 degrees
theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=.5))  

#import dataset into ggplot2
plt<- ggplot(mpg,aes(x=manufacturer, y=hwy))
#add boxplot
plt + geom_boxplot() +
  #rotate x-axis labels 45 degrees
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  #overlay scatter plot on top
  geom_point()

#create summary table
mpg_summary <- mpg %>% group_by(class) %>% summarize (Mean_Engine=mean(displ))
#import dataset into ggplot2
plt <- ggplot(mpg_summary, aes(x=class, y=Mean_Engine))
#add scatter plot
plt + geom_point(size=4) + labs(x="Vehicle Class", y="Mean Engine Size")

#import dataset into ggplot2
#provide context around standard deviation of engine size for each vehicle class
mpg_summary <- mpg %>% group_by(class) %>% summarize (Mean_Engine=mean(displ), SD_Engine=sd(displ))
plt <- ggplot(mpg_summary, aes(x=class, y=Mean_Engine))
#add scatter plot with labels
plt +geom_point(size=4) + labs(x="Vehicle Class", y="Mean Engine Size") + 
  #overlay with error bars
  geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine))

#convert to long format
mpg_long <- mpg %>% gather (key = "MPG_Type", value = "Rating", c(cty, hwy))
head(mpg_long)

#import dataset into ggplot2
plt <- ggplot(mpg_long, aes(x=manufacturer, y=Rating, color=MPG_Type))
# add boxplot with labels rotated 45 degrees
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45, hjust=1))

#import dataset into ggplot2
plt <- ggplot(mpg_long, aes(x=manufacturer, y=Rating, color=MPG_Type))
#create multiple boxplots, one for each MPG type
plt + geom_boxplot() + facet_wrap(vars(MPG_Type)) + 
  #rotate x-axis labels
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="none")+xlab ("Manufacturer")

#visualize distribution using density plot (qualitative)
ggplot(mtcars, aes(x=wt)) + geom_density()

#shapiro test
shapiro.test(mtcars$wt)

#sampling from used car dataset
#import used car dataset
population_table <- read.csv('used_car_data.csv', check.names=F, stringsAsFactors = F)
#import dataset into ggplot2
plt <- ggplot(population_table, aes(x=log10(Miles_Driven)))
#visualize distribution using density plot
plt + geom_density()

#one-sample t-test
#randomly sample 50 data points
sample_table <- population_table %>% sample_n(50)
#import dataset into ggplot2
plt <- ggplot(sample_table, aes(x=log10(Miles_Driven)))
#visualize distribution using density plot
plt+geom_density()
#compare sample versus population means
t.test(log10(sample_table$Miles_Driven), mu=mean(log10(population_table$Miles_Driven)))

#two- sample t-test
#generate 50 randomly sampled data points
sample_table <- population_table %>% sample_n(50)
#generate another 50 randomly sampled data points
sample_table2 <- population_table %>% sample_n(50)
#compare means of two samples
t.test(log10(sample_table$Miles_Driven), log10(sample_table2$Miles_Driven))

#paired t-test
#import dataset
mpg_data <- read.csv('mpg_modified.csv')
#select only data points where the year is 1999
mpg_1999 <- mpg_data %>% filter(year ==1999)
#select only data points where the year is 2008
mpg_2008 <- mpg_data %>% filter(year ==2008)
#compare the mean difference between two samples
t.test(mpg_1999$hwy, mpg_2008$hwy, paired=T)

#ANOVA testing
#filter columns from mtcars dataset
mtcars_filt <- mtcars[,c("hp", "cyl")]
#convert numeric column to factors
mtcars_filt$cyl <- factor(mtcars_filt$cyl)
#compare means across multiple levels
aov(hp ~ cyl, data=mtcars_filt)
#wrap aov() function in a summary()
summary(aov(hp~cyl, data=mtcars_filt))

#practice with Pearson correlation coefficient
head(mtcars)
#import dataset into ggplot2
plt <- ggplot(mtcars,aes(x=hp, y=qsec))
#create scatter plot
plt + geom_point()
#calculate correlation coefficient
cor(mtcars$hp, mtcars$qsec)

#another example of r-value
#read in dataset
used_cars <- read.csv('used_car_data.csv', stringsAsFactors = F)
head(used_cars)
#import dataset into ggplot2
plt <- ggplot(used_cars, aes(x=Miles_Driven, y=Selling_Price))
#create scatter plot
plt + geom_point()
#calculate correlation coefficient
cor(used_cars$Miles_Driven, used_cars$Selling_Price)

#correlation matrix
#convert dataframe into numeric matrix
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")])
cor(used_matrix)
           
#create linear regression model 
lm(qsec ~ hp, mtcars)
#summarize linear model
summary(lm(qsec~hp, mtcars))

#visualize fitted line against dataset
#create linear model
model <-lm(qsec ~ hp, mtcars)
#determine y-axis values from linear model
yvals <- model$coefficients['hp']*mtcars$hp + model$coefficients ['(Intercept)'] 
#import dataset into ggplot2
plt <- ggplot(mtcars, aes(x=hp, y=qsec))
#plot scatter and linear model
plt + geom_point() + geom_line(aes(y=yvals), color="red")

#multiple linear regression model
lm(qsec ~ mpg + disp + drat + wt + hp, data=mtcars)
#generate summary statistics
summary(lm(qsec ~ mpg + disp + drat + wt + hp, data=mtcars))

#chi-squared test
#generate contingency table
tbl<-table(mpg$class, mpg$year)
#compare categorical distributions
chisq.test(tbl)

head(used_cars)
