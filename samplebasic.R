popMean = 100; # The mean of our population
popSD = 15; # The standard deviation of our population
sampSize = 30; # The size of the samples we'll draw from the population

population <- rnorm(1000000, mean = popMean, sd = popSD)
hist(population)

mean(population)
sd(population)

sampChamp <- sample(population, sampSize, replace=TRUE)
mean(sampChamp)

sampDist <- replicate(10000,mean(sample(population, sampSize, replace=TRUE)))
hist(sampDist)

mean(sampDist)
sd(sampDist)

sdDist <- replicate(10000,(((sqrt(var(sample(population, sampSize, replace=TRUE)))*(sampSize-1))/(sampSize))))
hist(sdDist)

mean(sdDist)

sdDist_2 <- replicate(10000,sd(sample(population, sampSize, replace=TRUE)))
hist(sdDist_2)

mean(sdDist_2)
