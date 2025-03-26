cps09mar <- read.table('https://www.ssc.wisc.edu/~bhansen/econometrics/cps09mar.txt', header = FALSE, sep = "",dec = ".")
colnames(cps09mar) <- c("age","female","hisp","edu","earn","hours","week","union","uncov","region","race","marital")
N <- 5000
set.seed(12345)
selectedunits <- sample.int(n=length(cps09mar[,1]),size=N)
cps09mar <- cps09mar[selectedunits,]
rownames(cps09mar) <- rep(1:N)
tot <- cps09mar$hours*cps09mar$week
cps09mar$wage <- cps09mar$earn/tot
cps09mar$lme <- cps09mar$age-cps09mar$edu-6
cps09mar$lme2 <- cps09mar$lme^2

# Part 1 a 
model1 <- lm(wage ~ edu + lme + lme2, cps09mar)
summary(model1)

bhats <- coefficients(model1)

fitted <- fitted.values(model1)

res <- residuals(model1)

# Part 2
library(sandwich)


HC0 <- diag(vcovHC(model1, type = "HC0"))
HC1 <- diag(vcovHC(model1, type = "HC1"))
HC2 <- diag(vcovHC(model1, type = "HC2"))
HC3 <- diag(vcovHC(model1, type = "HC3"))
homo_var <- diag(vcov(model1))
table1 <- rbind(bhats, homo_var, HC0, HC1, HC2, HC3)
table1
# Part 3
B <- 1000

# Part 3 a)

OLS_edu <- lm(res^2 ~ cps09mar$edu)
res_2_edu <- (fitted.values(OLS_edu))

e_sim <- rnorm(length(res_2_edu), 0, sqrt(abs((res_2_edu))))

estimates <- matrix(NA, nrow = 1000, ncol = 4)
homo_var_sim <- matrix(NA, nrow = 1000, ncol = 4)
HC0 <- matrix(NA, nrow = 1000, ncol = 4)
HC1 <- matrix(NA, nrow = 1000, ncol = 4)
HC2 <- matrix(NA, nrow = 1000, ncol = 4)
HC3 <- matrix(NA, nrow = 1000, ncol = 4)

for(i in 1:B){
  e_sim <- rnorm(length(res_2_edu), 0, sqrt(abs(res_2_edu)))
  wage_sim <- fitted + e_sim
  
  model <- lm(wage_sim ~ cps09mar$edu + cps09mar$lme + cps09mar$lme2)
  
  estimates[i,] <- coefficients(model)
  
  homo_var_sim[i,] <- diag(vcov(model))
  HC0[i,] <- diag(vcovHC(model, type = c("HC0")))
  HC1[i,] <- diag(vcovHC(model, type = c("HC1")))
  HC2[i,] <- diag(vcovHC(model, type = c("HC2")))
  HC3[i,] <- diag(vcovHC(model, type = c("HC3")))
  
}
mean_estimates <- colMeans(estimates) 
mean_homo_var_sim <- colMeans(homo_var_sim)
mean_HC0 <- colMeans(HC0)
mean_HC1 <- colMeans(HC1)
mean_HC2 <- colMeans(HC2)
mean_HC3 <- colMeans(HC3)

table2 <- rbind(mean_estimates, mean_homo_var_sim, mean_HC0, mean_HC1, mean_HC2, mean_HC3)
table2

# Part 3 b)

model3 <- lm(res^2 ~ cps09mar$edu + cps09mar$lme + cps09mar$lme2 + cps09mar$edu*cps09mar$lme + cps09mar$edu*cps09mar$lme2)
res_fitted_3 <- fitted.values(model3)


estimates_3 <- matrix(NA, nrow = 1000, ncol = 4)
homo_var_sim_3 <- matrix(NA, nrow = 1000, ncol = 4)
HC0_3 <- matrix(NA, nrow = 1000, ncol = 4)
HC1_3 <- matrix(NA, nrow = 1000, ncol = 4)
HC2_3 <- matrix(NA, nrow = 1000, ncol = 4)
HC3_3 <- matrix(NA, nrow = 1000, ncol = 4)


for(i in 1:B){
  e_sim_3 <- rnorm(length(res_fitted_3), 0 , sqrt(abs(res_fitted_3)))
  wage_sim_3 <- fitted + e_sim_3
  
  model3 <- lm(wage_sim_3 ~ cps09mar$edu + cps09mar$lme + cps09mar$lme2)
  
  estimates_3[i,] <- coefficients(model3)
  
  homo_var_sim_3[i,] <- diag(vcov(model3))
  HC0_3[i,] <- diag(vcovHC(model3, type = c("HC0")))
  HC1_3[i,] <- diag(vcovHC(model3, type = c("HC1")))
  HC2_3[i,] <- diag(vcovHC(model3, type = c("HC2")))
  HC3_3[i,] <- diag(vcovHC(model3, type = c("HC3")))
}

mean_estimates_3 <- colMeans(estimates_3) 
mean_homo_var_sim_3 <- colMeans(homo_var_sim_3)
mean_HC0_3 <- colMeans(HC0_3)
mean_HC1_3 <- colMeans(HC1_3)
mean_HC2_3 <- colMeans(HC2_3)
mean_HC3_3 <- colMeans(HC3_3)

table3 <- rbind(mean_estimates_3, mean_homo_var_sim_3, mean_HC0_3, mean_HC1_3, mean_HC2_3, mean_HC3_3)
table3                



table1
table2
table3
# Part 4
# Benchmark
benchmark <- mean(cps09mar$wage)

j <- 20

y_bar <- numeric()
beta_OLS <- numeric(4)
beta_WLS <- numeric(4)

pred_OLS <- numeric()
pred_WLS <- numeric()

SSR_mean <- numeric()
SSR_OLS <- numeric()
SSR_WLS <- numeric()

library(dplyr)
for(i in 1:j){
  train <- sample.int(n = nrow(cps09mar), size = 10*i, replace = FALSE)
  train_data <- cps09mar[train, ]
  test_data <- cps09mar[-train, ]
  
  y_bar <- mean(train_data$wage)
  
  OLS_model <- lm(wage ~ edu + lme + lme2, train_data)
  beta_OLS <- coefficients(OLS_model)
  
  res_OLS <- residuals(OLS_model)
  weights <- 1 / (res_OLS)^2
  
  WLS_model <- lm(wage ~ edu + lme + lme2, train_data, weights = weights)
  beta_WLS <- coefficients(WLS_model)
  
  subset_test_data <- test_data[, c("edu", "lme", "lme2")]
  subset_test_data <- cbind(intercept = 1, subset_test_data)
  
  pred_OLS <- predict(OLS_model, subset_test_data)
  pred_WLS <- predict(WLS_model, subset_test_data)
  
  
  SSR_mean[i] <-sum((y_bar - test_data$wage)^2)  
  SSR_OLS[i] <- sum((pred_OLS - test_data$wage)^2)
  SSR_WLS[i] <- sum((pred_WLS - test_data$wage)^2)
  
  
}
y_bar
beta_OLS
beta_WLS

SSR_mean
SSR_OLS
SSR_WLS


# Create df for results
ssr_results <- data.frame(
  train_size = seq(10, 200, by = 10),  # x-axis
  SSR_Mean = log(SSR_mean),  
  SSR_OLS = log(SSR_OLS),    
  SSR_WLS = log(SSR_WLS)     
)

# Change to long format
library(tidyr)
ssr_long <- pivot_longer(ssr_results, cols = c("SSR_Mean", "SSR_OLS", "SSR_WLS"), 
                         names_to = "Model", values_to = "SSR")

colors <- c("SSR_OLS" = "blue", "SSR_WLS" = "red", "SSR_Mean" = "green")

ggplot(ssr_long, aes(x = train_size, y = SSR, color = Model)) +
  geom_line(size = 1) +  
  geom_point(size = 2, shape = 1) +  
  scale_color_manual(values = colors) +
  labs(title = "SSR vs Training Size",
       x = "Training Size",
       y = "Sum of Squared Residuals (SSR)",
       color = "Model")




# Part 4. 6 #
j <- 100

y_bar <- numeric()
beta_OLS <- numeric(4)
beta_WLS <- numeric(4)

pred_OLS <- numeric()
pred_WLS <- numeric()

SSR_mean_6 <- numeric()
SSR_OLS_6 <- numeric()
SSR_WLS_6 <- numeric()

library(dplyr)
for(i in 21:j){
  train <- sample.int(n = nrow(cps09mar), size = 10*i, replace = FALSE)
  train_data <- cps09mar[train, ]
  test_data <- cps09mar[-train, ]
  
  y_bar <- mean(train_data$wage)
  
  OLS_model <- lm(wage ~ edu + lme + lme2, train_data)
  beta_OLS <- coefficients(OLS_model)
  
  res_OLS <- residuals(OLS_model)
  weights <- 1 / (res_OLS)^2
  
  WLS_model <- lm(wage ~ edu + lme + lme2, train_data, weights = weights)
  beta_WLS <- coefficients(WLS_model)
  
  subset_test_data <- test_data[, c("edu", "lme", "lme2")]
  subset_test_data <- cbind(intercept = 1, subset_test_data)
  
  pred_OLS <- predict(OLS_model, subset_test_data)
  pred_WLS <- predict(WLS_model, subset_test_data)
  
  
  SSR_mean_6[i] <-sum((y_bar - test_data$wage)^2)  
  SSR_OLS_6[i] <- sum((pred_OLS - test_data$wage)^2)
  SSR_WLS_6[i] <- sum((pred_WLS - test_data$wage)^2)
  
  
}
y_bar
beta_OLS
beta_WLS

SSR_mean_6
SSR_OLS_6
SSR_WLS_6


# Create df for results
ssr_results <- data.frame(
  train_size = seq(10, 200, by = 10),  # x-axis
  SSR_Mean = log(SSR_mean),  
  SSR_OLS = log(SSR_OLS),    
  SSR_WLS = log(SSR_WLS)     
)

# Change to long format
library(tidyr)
ssr_long <- pivot_longer(ssr_results, cols = c("SSR_Mean", "SSR_OLS", "SSR_WLS"), 
                         names_to = "Model", values_to = "SSR")

colors <- c("SSR_OLS" = "blue", "SSR_WLS" = "red", "SSR_Mean" = "green")

ggplot(ssr_long, aes(x = train_size, y = SSR, color = Model)) +
  geom_line(size = 1) +  
  geom_point(size = 2, shape = 1) +  
  scale_color_manual(values = colors) +
  labs(title = "SSR vs Training Size",
       x = "Training Size",
       y = "Sum of Squared Residuals (SSR)",
       color = "Model")












