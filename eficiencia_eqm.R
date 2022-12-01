library(tidyverse)
library(patchwork)
set.seed(432354675)
# Functions to compute the estimators
T1 <- function(x) 2 * mean(x) - 1
T2 <- function(x) ( (length(x) + 1)/length(x) ) * max(x) - 1

# Now, simulate in order to assess their bias.
# This goes as follows (try this yourself before looking):
# - Choose a true value of N, the parameter to be estimated
# - Draw a sample of size n from 1:N without replacement
# - Compute T1 and T2
# - Repeat this M times, and compare the average of T1 and T2 to N.
N <- 1000
n <- 100
M <- 1000 # One million simulations
# Run the simulations. Use the sample.int() function to generate from a DISCRETE
# uniform distribution
thesimulations <- list(
  T1 = numeric(M),
  T2 = numeric(M),
  T3 = numeric(M)
)
for (i in 1:M) {
  # Do the simulation
  # Sample from a discrete uniform (?sample.int):
  thesample <- sample.int(N,n,replace = FALSE)
  # Record the values of the two estimators:
  thesimulations$T1[i] <- T1(thesample)
  thesimulations$T2[i] <- T2(thesample)
}

# Evaluate the bias of T1 and T2:
mean(thesimulations$T1) - N

mean(thesimulations$T2) - N


# Recreate the plots in Figure 20.1:
leftplot <- tibble(T1 = thesimulations$T1) %>%
  ggplot(aes(x = T1)) +
  theme_classic() + 
  geom_histogram(aes(y = ..density..),bins = 30,colour = "black",fill = "transparent") +
  scale_x_continuous(breaks = c(300,700,1000,1300,1600)) +
  coord_cartesian(xlim = c(300,1600))

rightplot <- tibble(T2 = thesimulations$T2) %>%
  ggplot(aes(x = T2)) +
  theme_classic() + 
  geom_histogram(aes(y = ..density..),bins = 30,colour = "black",fill = "transparent") +
  scale_x_continuous(breaks = c(300,700,1000,1300,1600)) +
  coord_cartesian(xlim = c(300,1600))

leftplot | rightplot


var(thesimulations$T1)

var(thesimulations$T2)



T3 <- function(x) max(x)
mse <- function(x) var(x) + (mean(x) - N)^2

thesimulations$T3 <- numeric(M)
for (i in 1:M) {
  thesample <- sample.int(N,n)
  thesimulations$T3[i] <- T3(thesample)
}

mean(thesimulations$T1) - N
mean(thesimulations$T2) - N
mean(thesimulations$T3) - N

var(thesimulations$T1)
var(thesimulations$T2)
var(thesimulations$T3)


mse(thesimulations$T1)
mse(thesimulations$T2)
mse(thesimulations$T3)
