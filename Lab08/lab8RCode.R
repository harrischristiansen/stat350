################ Problem A ###############

sales <- read.table(file="sales.txt", header=TRUE)

################ Part 1 ###############

attach(sales)
library(lattice)
xyplot(SalesPrice ~ AssessedValue, data = sales, panel=function(x,y) {
	panel.xyplot(x, y)
	panel.lmline(x, y)
}, main="Sales Data Scatterplot")

################ Part 3 ###############
cor(sales$SalesPrice, sales$AssessedValue)

################ Part 5 ###############

sales.lm = lm(SalesPrice ~ AssessedValue)
sales.res = sales.lm$res
summary(sales.lm)
# Answer: SalesPrice = 66.9460 + 0.6819AssesedValue
# R^2: 0.4987

################ Part 6 ###############

# Answer (Property=1): 66.9460 + 0.6819*188.7 = 195.6205
# TO-DO: Residual

################ Part 7 ###############

xyplot(sales.res ~ AssessedValue, data=sales, main="Residual Plot For Sales", ylab="Residual", panel=function(x,y){
	panel.xyplot(x,y)
	panel.abline(h=0)
})

################ Part 8 ###############

hist(sales.res, main="Histogram of Residuals for Sales")
qqnorm(sales.res, main="QQPlot of Residuals for Sales")
qqline(sales.res)

################ Part 10 ###############

confint(sales.lm, level=0.99)