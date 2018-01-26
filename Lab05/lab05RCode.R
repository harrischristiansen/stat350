##### Problem A #####
friendData <- read.table(file="facebookfriends.txt",header=T)

##### Part 1 #####
boxplot(friendData$Friends,main="Facebook Friends Boxplot") #Boxplot
hist(friendData$Friends,main="Facebook Friends Histogram") # Histogram
# QQplot
qqnorm(friendData$Friends,main="Facebook Friends QQPlot")
qqline(friendData$Friends)

##### Part 3 #####
mean(friendData$Friends) # Mean
sd(friendData$Friends) # SD
sd(friendData$Friends)/sqrt(length(friendData$Friends)) # Standard Error

##### Part 4 #####
t.test(friendData$Friends, conf.level=0.95, mu = 130)

##### Problem B #####
pickData <- read.table(file="pickcount.txt",header=T)

##### Part 1 #####
boxplot(pickData$PickCount,main="Pick Count Boxplot") #Boxplot
hist(pickData$PickCount,main="Pick Count Histogram") # Histogram
# QQplot
qqnorm(pickData$PickCount,main="Pick Count QQPlot")
qqline(pickData$PickCount)

##### Part 4 #####
mean(pickData$PickCount) # Mean
sd(pickData$PickCount) # SD
sd(pickData$PickCount)/sqrt(length(pickData$PickCount)) # Standard Error

##### Part 5 #####
t.test(pickData$PickCount, conf.level=0.95, mu = 130, alternative="greater")