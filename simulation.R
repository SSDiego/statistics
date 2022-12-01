# Simulação

# X1, ..., Xn Amostra de tamanho n = 30
# X ~ Pois(lambda) com lambda = log 10
# queremos estimar p0, X = 0

# Então p0 = P(X = 0) = e^-lambda (densidade da poisson para x = 0)
# Como a Esperança da Poisson é lambda, E(X) = log 10

# Estimadores

  # S = 1/n * sum(Xi = 0) (Corresponde a propoção de Xi = 0)
  # T = e^-Xbarran (Corresponde a estimar a média da população)

set.seed(6574564)
# Simulate 500 random samples of size 30 from a poisson(log(10))
N <- 500
n <- 30
lambda <- log(10)
p0 <- exp(-lambda) # True value of p0

# Write functions to compute each estimator
compute_S <- function(samp) mean(samp == 0)
compute_T <- function(samp) exp(-mean(samp))

# Simulate the samples and calculate the estimators for each sample
samples <- vector(mode = "list",length = N)
SS <- TT <- numeric(N)
for (i in 1:N) {
  samples[[i]] <- rpois(n,lambda)
  SS[i] <- compute_S(samples[[i]])
  TT[i] <- compute_T(samples[[i]])
}


# Create the plots
plt_S <- tibble(SS = SS) %>%
  ggplot(aes(x = SS)) +
  theme_classic() +
  geom_histogram(colour = "black",fill = "transparent",bins = 7) +
  coord_cartesian(ylim = c(0,250)) +
  geom_vline(xintercept = p0,colour = "red",linetype = "dotdash")

plt_T <- tibble(TT = TT) %>%
  ggplot(aes(x = TT)) +
  theme_classic() +
  geom_histogram(colour = "black",fill = "transparent",bins = 7) +
  coord_cartesian(ylim = c(0,250)) +
  geom_vline(xintercept = p0,colour = "red",linetype = "dotdash")

plt_S | plt_T

cowplot::plot_grid(plt_S,plt_T,nrow = 1)

# Compute the mean of each:
mean(SS)
mean(TT)

summary(SS) 
summary(TT)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(SS)

max(table(SS))
