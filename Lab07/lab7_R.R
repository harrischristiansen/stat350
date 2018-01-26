##### Problem A #####
bmdData <- read.table(file="ex12-45bmd.txt",header=T)

##### Part 1 #####
boxplot(BMD~Treatment,bmdData,main="BMD Data Boxplot") # Boxplot
# Effects Plot
trace <-rep(1,length(bmdData$Treatment))
interaction.plot(bmdData$Treatment,trace,bmdData$BMD,fun=mean,lengend=F,main="BMD Effects Plot")
# Info Table
attach(bmdData)
tapply(BMD, Treatment, length) # Sample Size
tapply(BMD, Treatment, mean) # Mean
tapply(BMD, Treatment, sd) # Standard Deviation

##### Part 2 #####
library(lattice)
qqmath(~BMD | Treatment, data = bmdData, layout=c(3,1), panel = function(x) {
	panel.qqmath(x)
	panel.qqmathline(x)
	}, main="Problem A Part 2")

##### Part 3 #####
fit <- aov(BMD ~ Treatment, bmdData)
summary(fit)

##### Part 4 #####
pairwise.t.test(bmdData$BMD, bmdData$Treatment, p.adjust="bon")

##### Problem B #####
boneData <- read.table(file="ex12-47jump.txt",header=T)

##### Part 1 #####
boxplot(density~group,boneData,main="Bone Data Boxplot") # Boxplot
# Effects Plot
trace <-rep(1,length(boneData$group))
interaction.plot(boneData$group,trace,boneData$density,fun=mean,lengend=F,main="Bone Data Effects Plot")
# Info Table
attach(boneData)
tapply(density, group, length) # Sample Size
tapply(density, group, mean) # Mean
tapply(density, group, sd) # Standard Deviation

##### Part 2 #####
library(lattice)
qqmath(~density | group, data = boneData, layout=c(3,1), panel = function(x) {
	panel.qqmath(x)
	panel.qqmathline(x)
	}, main="Problem B Part 2")

##### Part 3 #####
fit <- aov(density ~ group, boneData)
summary(fit)

##### Part 4 #####
test.Tukey <- TukeyHSD(fit, conf.level=0.95)
test.Tukey
plot(test.Tukey)