sampleSize <- 50 # Sample Size For Each Trial
numSamples <- 1000 # Number of Samples
title <- paste("Exponential Distribution - Sample Size", sampleSize)

samples.means <- rep(0,numSamples) # Create samples.means array
for (i in 1:numSamples) { # Generate numSamples distributions
	dist <- rexp(sampleSize,rate=2) # Exp distribution with sampleSize
	samples.means[i] = mean(dist) # Calculate + Save Mean
}

hist(samples.means,main=title) # Generate Histogram

qqnorm(samples.means,main=title) # Generate Normal Quantile Plot

mean(samples.means) # Print Mean

sd(samples.means) # Print Standard Deviation