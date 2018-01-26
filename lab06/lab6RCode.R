##### Problem A #####

##### Part 2 #####
weightData <- read.table(file="wtgain.txt",header=T)
weightDiff = weightData$wta - weightData$wtb
boxplot(weightDiff,main="Weight Change Boxplot") #Boxplot
hist(weightDiff,main="Weight Change Histogram") # Histogram
# QQPlot
qqnorm(weightDiff,main="Weight Change QQPlot")
qqline(weightDiff)

##### Part 3 #####
t.test(weightData$wta, weightData$wtb, conf.level=0.95, paired=TRUE, alternative="two.sided")

##### Problem B 4 #####

##### Part 2 #####
homeData <- read.table(file="houseprice.txt",header=T)
attach(homeData)
threeBed <- subset(homeData, Bedroom=="3")
fourBed <- subset(homeData, Bedroom=="4")
## 3 Bedroom
boxplot(threeBed$Price,main="Three Bedroom House Boxplot") #Boxplot
hist(threeBed$Price,main="Three Bedroom House Histogram") # Histogram
# QQPlot
qqnorm(threeBed$Price,main="Three Bedroom House QQPlot")
qqline(threeBed$Price)
## 4 Bedroom
boxplot(fourBed$Price,main="Four Bedroom House Boxplot") #Boxplot
hist(fourBed$Price,main="Four Bedroom House Histogram") # Histogram
# QQPlot
qqnorm(fourBed$Price,main="Four Bedroom House QQPlot")
qqline(fourBed$Price)

##### Part 5 #####
t.test(threeBed$Price, fourBed$Price, conf.level=0.99, paired=F, alternative = "less", var.equal=F)