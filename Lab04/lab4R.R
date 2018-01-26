n <- 40
result = matrix(0,30,2)
for(i in 1:30) {
	rData <- rnorm(n,10,2)
	t=t.test(rData, conf.level=0.95)
	result[i,] = t$conf[1:2]
}

n <- 1:20
alpha <- 0.01
muTest = 6.3

mu <- 6
sd <- 0.25
sdError <- sd/sqrt(n)
z <- qnorm(1 - alpha/2)
x1 <- mu - z*sdError
x2 <- mu + z*sdError
px1 <- pnorm(x1, muTest, sdError)
px2 <- pnorm(x2, muTest, sdError, lower.tail = FALSE)
power <- px1 + px2
answer <- data.frame(n, power)
answer