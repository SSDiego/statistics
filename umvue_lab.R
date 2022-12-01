#2X(bar) is an unbiased estimator of θ. Taking n = 100, perform a simulation study using θ = 5
#that compares the UMVUE with 2X in terms of mean squared error. Use at least a thousand
#repetitions in your study.


#The following code generates a matrix of 1000 samples of size n = 100 from uniform(0; 5):
n <- 100; reps <- 1000
set.seed(321)
m <- matrix(5*runif(n*reps), n, reps)

#Now we apply both estimators to the samples, which are in the columns of the matrix:
est1 <- (n+1)/n*apply(m, 2, max)
est1
est2 <- 2*apply(m, 2, mean)
est2
c(UMVU=mean((est1-5)^2), alternative=mean((est2-5)^2))



#The (unbiased version of the) sample variance, S2, is an unbiased estimator of θ2=12.
#Conduct a simulation study similar to the one in part (d), also with n = 100 and θ = 5, in which
#you compare the UMVUE of θ2 to 12S2

est3 <- (n+2)/n*apply(m, 2, max)^2
est3
est4 <- 12*apply(m, 2, var)
est4
c(UMVU=mean((est3-25)^2), alternative=mean((est4-25)^2))


