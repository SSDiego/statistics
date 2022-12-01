library(dplyr)
library(MASS)
library(readxl)

# plot the chi_12^2 distribution
curve(dchisq(x, df=12), 
      from = 0, 
      to = 40, 
      ylab = "density", 
      xlab = "hourly earnings in Euro")

# set seed for reproducibility
set.seed(1)

# sample from the chi_12^2 distribution, keep only the first observation
rchisq(n = 100, df = 12)[1]

# generate a fictious population
pop <- rnorm(10000, 10, 1)

# sample from the population and estimate the mean
est1 <- replicate(expr = mean(sample(x = pop, size = 5)), n = 25000)

est2 <- replicate(expr = mean(sample(x = pop, size = 25)), n = 25000)

fo <- replicate(expr = sample(x = pop, size = 5)[1], n = 25000)

is.vector(est1)

is.vector(est2)

length(est1)

length(est2)

# plot density estimate Y_1
plot(density(fo), 
     col = 'green', 
     lwd = 2,
     ylim = c(0, 2),
     xlab = 'estimates',
     main = 'Sampling Distributions of Unbiased Estimators')

# add density estimate for the distribution of the sample mean with n=5 to the plot
lines(density(est1), 
      col = 'steelblue', 
      lwd = 2, 
      bty = 'l')

# add density estimate for the distribution of the sample mean with n=25 to the plot
lines(density(est2), 
      col = 'red2', 
      lwd = 2)

# add a vertical line at the true parameter
abline(v = 10, lty = 2)

# add N(10,1) density to the plot
curve(dnorm(x, mean = 10), 
      lwd = 2,
      lty = 2,
      add = T)

# add a legend
legend("topleft",
       legend = c("N(10,1)",
                  expression(Y[1]),
                  expression(bar(Y) ~ n == 5),
                  expression(bar(Y) ~ n == 25)
       ), 
       lty = c(2, 1, 1, 1), 
       col = c('black','green', 'steelblue', 'red2'),
       lwd = 2)

sqm <- function(m) {
  sum((y-m)^2)
}
sqm <- Vectorize(sqm)

# draw random sample and compute the mean
y <- rnorm(100, 10, 1)
mean(y)

# plot the objective function
curve(sqm(x), 
      from = -50, 
      to = 70,
      xlab = "m",
      ylab = "sqm(m)")

# add vertical line at mean(y)
abline(v = mean(y), 
       lty = 2, 
       col = "darkred")

# add annotation at mean(y)
text(x = mean(y), 
     y = 0, 
     labels = paste(round(mean(y), 2)))

mean(pop)


# simulate outcomes for the sample mean when the i.i.d. assumption fails
est3 <-  replicate(n = 25000, 
                   expr = mean(sample(x = sort(pop), 
                                      size = 10, 
                                      prob = c(rep(4, 2500), rep(1, 7500)))))

# compute the sample mean of the outcomes
mean(est3)

# sampling distribution of sample mean, i.i.d. holds, n=25
plot(density(est2), 
     col = 'steelblue',
     lwd = 2,
     xlim = c(8, 11),
     xlab = 'Estimates',
     main = 'When the i.i.d. Assumption Fails')

# sampling distribution of sample mean, i.i.d. fails, n=25
lines(density(est3),
      col = 'red2',
      lwd = 2)

# add a legend
legend("topleft",
       legend = c(expression(bar(Y)[n == 25]~", i.i.d. fails"),
                  expression(bar(Y)[n == 25]~", i.i.d. holds")
       ), 
       lty = c(1, 1), 
       col = c('red2', 'steelblue'),
       lwd = 2)
