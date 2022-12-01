library(dplyr)
library(ggplot2)
dhyper(0:3, 3, 5, 4) * 70

data_for_sampling <- data.frame(X = rep(0:3, times = 3), Y = rep(0:2, each = 4))
data_for_sampling <- mutate(data_for_sampling, probs = c(0, 3, 9, 3, 2, 18, 18, 2, 3, 9, 3, 0))

sampled_data <- sample_n(data_for_sampling, 
                         size = 10000, 
                         replace = T, 
                         weight = data_for_sampling$probs)

sampled_data <- select(sampled_data, -probs)

sampled_data %>% 
  count(X) %>% 
  mutate(prob = n/sum(n))

sampled_data %>% 
  filter(Y == 0) %>% 
  count(X) %>% 
  mutate(prob = n/sum(n))

sampled_data %>% 
  summarize(expected_x = mean(X))

sampled_data %>% 
  summarize(expected_xy = mean(X * Y))

cov(sampled_data$X, sampled_data$Y)


ggplot(data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = ~dbeta(.x, 2, 2))

xs <- runif(50000, 0, 1)
probs <- dbeta(xs, 2, 2)
random_sample <- sample(xs, 10000, replace = T, prob = probs)

hist(random_sample, probability = T)
curve(dbeta(x, 2, 2), add = T)

hist(rbeta(10000, 2, 2), probability = T)
curve(dbeta(x, 2, 2), add = T)

xs <- runif(50000, -4, 4)
probs <- dnorm(xs)
random_sample <- sample(xs, 10000, T, prob = probs)
hist(random_sample, probability = T)
curve(dnorm(x), add = T)

xs <- runif(50000, 0, 1)
ys <- runif(50000, 0, 1)
dat <- matrix(c(xs, ys), ncol = 2)
dat <- data.frame(dat)
names(dat) <- c("x", "y")

joint_pdf <- function(x, y) {
  x + y
}
probs <- joint_pdf(xs, ys)
indices <- sample(1:50000, 10000, T, probs)
random_sample <- dat[indices, ]
ggplot(random_sample, aes(x, y)) +
  geom_density_2d()

N <- 500000
xs <- runif(N, -4, 4)
ys <- runif(N, 0, 10)
dat <- matrix(c(xs, ys), ncol = 2)
dat <- data.frame(dat)
names(dat) <- c("x", "y")

joint_pdf <- function(x, y) {
  dnorm(x) * dexp(y)
}
probs <- joint_pdf(xs, ys)

indices <- sample(1:N, 10000, T, probs)
random_sample <- dat[indices,]
ggplot(random_sample, aes(x, y)) +
  geom_density_2d()