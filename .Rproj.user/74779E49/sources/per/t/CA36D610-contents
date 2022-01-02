# Bootstrapping
library(tidyverse)
library(ISLR2)
library(boot)

# Data: the portfolio data set is simulated data for 100 pairs of return,
# generated in the fashion
data("Portfolio")

alpha_fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

alpha_fn(Portfolio, 1:100)

# Sampling with replacement
set.seed(7)
alpha_fn(Portfolio, sample(100, 100, replace = T)) # Bootstrap is implemented by performing this command many times

# Bootstrapping using boot() function from boot library
boot(Portfolio, alpha_fn, R = 1000)

# Estimating the Accuracy of a Linear Regression Model
data("Auto")
# The intercept and slope terms for the linear regression
# model that uses horsepower to predict mpg in the Auto data set

# We will compare the estimates obtained using the bootstrap to those obtained
# using the formulas for SE(B0) and SE(B1)
boot_fn <- function(data, index) coef(lm(mpg ~ horsepower, data = data, subset = index))
boot_fn(Auto, 1:392)

set.seed(1)
boot_fn(Auto, sample(392, 392, replace = T))

boot(Auto, boot_fn, 1000)
summary(lm(mpg ~ horsepower, data = Auto))$coef

plot(Auto$horsepower, Auto$mpg) # Evaluate the model using quadratic model

boot_fn <- function(data, index) coef(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))
set.seed(1)
boot(Auto, boot_fn, 1000)

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
