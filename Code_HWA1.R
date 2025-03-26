library(ggplot2)
library(tidyverse)
library(stargazer)
library(ggplot2)

#### Exercise 1 ####
####  1 a) ####
# Importing the data
x <- c(4.06, 3.63, 0.41, 2.45, 0.22, 3.39, 0.27, 0.69, 0.32,
       0.09, 4.00, 3.63, 2.65, 3.06, 3.77, 2.29, 2.31, 1.51)

y <- c(168.44, 178.66, 58.81, 189.29, 14.79, 178.70, 103.69, 103.30, 57.30,
       15.10, 172.28, 172.30, 194.32, 193.20, 167.50, 191.50, 173.98, 174.60)
# Adding intercept to x and making it to matrix with both x values the the log x values
X <- cbind(1, x, log(x))  

# Changing y to matrix form and taking the log
Y <- matrix(log(y))

# Compute estimated beta coefficients
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_hat)

# Just to check with built in function in R if it gives same answer
model <- lm(log(y) ~ x + log(x))
summary(model)
##### c) ####
beta0_hat <- beta_hat[1]
beta1_hat <- beta_hat[2]
beta2_hat <- beta_hat[3]

#### 1 b) ####
#y_hat <- beta_hat[1] + beta_hat[2] * x + beta_hat[3] * log(x)
y_hat <- exp(beta0_hat) * exp(beta1_hat * x) * (x^beta2_hat) # page 70 in book
#log(y_hat)

df <- data.frame(x, y, y_hat)

ggplot(data = df) + 
  geom_point(aes(x = x, y = y), color = "forest green", size = 2) +  
  geom_line(aes(x = x, y = y_hat), color = "black")  +
  labs(title = "Scatterplot of data together with the fitted regression model line", x = "x", y = "y")
  

#### Exercise 2 ####
rm(list = ls())
data <- read.table("cps09mar.txt", header = FALSE, sep = "\t")
head(data)

beta0 <- 1
beta1 <- 2
beta2 <- 5
sigma2 <- 1
set.seed(1)
epsilon <- rnorm(100, mean = 0, sqrt(sigma2))

X <- cbind(1, data[, 5], data[, 4])
n <- 100

X_random_100 <- apply(X, 2, function(col) sample(col, size = 1000, replace = FALSE))


y_sim <- beta0 + beta1*X_random_100[,2] + beta2* X_random_100[, 3] + epsilon

#### 2 a) ####
s = 300
# for loop for the simulation

# storage for beta1
beta1_hat_miss_list <- numeric(s)

# Simulation 300 times
for (i in 1:s){

  # initiate new epsilon
  epsilon <- rnorm(100, mean = 0, sqrt(sigma2))
  # new simulated y
  y_sim <- beta0 + beta1*X_random_100[,2] + beta2* X_random_100[, 3] + epsilon
  
  # compute new matrix without x_2
  X_miss <- cbind(X_random_100[,1], X_random_100[,2])
  # estimating betas
  beta_hat_miss <- solve(t(X_miss) %*% X_miss) %*% t(X_miss) %*% y_sim
  # adding into list of storage of estimated beta1
  beta1_hat_miss_list[i] <- beta_hat_miss[2,] 
}
beta1_hat_miss_list
# taking mean of all beta1
beta1_mean <- mean(beta1_hat_miss_list)
# displaying it in a histogram
hist(beta1_hat_miss_list, main = "Distribution of the estimated beta1 from the simulation",xlab = "Estimates of Beta1")

#### 2 b) ####

# computing the SD of beta1
se_beta1 <- sd(beta1_hat_miss_list) / sqrt(s)
print(se_beta1)

# confidence interval with alpha set to 0.05
ci_lower <- beta1_mean - 1.96000000 * se_beta1
ci_upper <- beta1_mean + 1.96000000 * se_beta1



print(ci_lower, tol = 100000000000)
print(ci_upper, tol = 100000000000)



