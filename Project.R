install.packages("tidyverse")
library(tidyverse)

install.packages("devtools")
library(devtools)
library(randomForest)
install.packages("dplyr")
library("dplyr")
install.packages("skimr")
library("skimr")
install.packages("caret")
library("caret")
install.packages("vip")
library("vip")
library("ggplot2")
install.packages("lattice")
library("lattice")

#### Project Report ####
library(ggplot2)
library(ggcorrplot)
library(ggstance)
library(dplyr)
library(pROC)

#### Importing Data ####
df <- loan_data
nrow(df)
#### Modifying Variables to binary 0 and 1 instead of text
#### Modifying Variables to binary 0 and 1 instead of text
df$Education <- ifelse(df$Education == "Graduate", 1, 0)
df$Loan_Status <- ifelse(df$Loan_Status == "Y", 1, 0)
df$Self_Employed<- ifelse(df$Self_Employed == "Yes", 1, 0)
df$Married <- ifelse(df$Married == "Yes", 1, 0)
df$Gender <- ifelse(df$Gender == "Male", 1, 0)


#### Exploratory Data analysis 
#### Correlation Matrix ####

df_corr <- df %>% drop_na() %>%  select(-Loan_ID) %>%  select(-Property_Area) %>%  select(-Dependents)
corr_matrix <- cor(df_corr)
df_corr

# Correlation plot
ggcorrplot(corr_matrix,
           method = "square",  
           type = "lower",     
           lab = TRUE,         
           colors = c("blue", "white", "red"), 
           hc.order = TRUE,
           theme_classic())    
# Boxplot to visualize outliers for variables on interval scale
df_boxplot <- df %>% select(c(ApplicantIncome, CoapplicantIncome, LoanAmount, 
                              Loan_Amount_Term))

# Change to long format
df_long <- df_boxplot %>%
  pivot_longer(
    cols = everything(),            
    names_to = "Variable",          
    values_to = "Value"  )           


# Create facetted boxp
ggplot(df_long, aes(x = Variable, y = Value)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~ Variable, scales = "free_y") +  # seperate y scales
  labs(title = "Facetterad boxplot", x = "Variabler", y = "Värden") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  


#### Random Forest ####
install.packages("randomForest")
library(randomForest)
set.seed(123) 

# Clean data from na and ID variable
df_cleaned <- df %>% drop_na() %>% select(-Loan_ID)


# Split data into train and test data using a 80/20 split
train_index <- sample(nrow(df_cleaned), 0.8 * nrow(df_cleaned), replace = FALSE) 
train_data <- df_cleaned[train_index, ] 
test_data <- df_cleaned[-train_index, ] 

# Change Loan_Status to factor so that randomforest makes a classification an dnot numeric regression
train_data$Loan_Status <- as.factor(train_data$Loan_Status)

# Create randomforest model
set.seed(123)
rf <- randomForest(Loan_Status ~ ., data = train_data, ntree = 500, mtry= 3, 
                   importance = TRUE, maxnodes = 7)
# Evaluate OOB
print(rf)

# Compute its predictions for training data
pred_train <- predict(rf, train_data, type = "class")

# Display its predictions as confusion matrix for training data
table(pred_train, train_data$Loan_Status)
# Calculating accuracy
mean(pred_train == train_data$Loan_Status)


# Compute its predictions for test data
pred_test <- predict(rf, test_data, type = "class")
# Display its predictions as confusion matrix for test data
table(pred_test, test_data$Loan_Status)
# Calculating accuracy for test data
mean(pred_test == test_data$Loan_Status)

# Calculating Baseline Accuracy 
baseline_acc <- max(table(test_data$Loan_Status)) / nrow(test_data)
baseline_acc





vip <- vip(rf, type = 1)
print(vip)

# ROC Curve ##
# Load the pROC package
library(pROC)

# Combine classes 1 and 2 as positive class and convert to binary format
pred_binary <- ifelse(pred_test == 0, 0, 1)
true_binary <- ifelse(test_data$Loan_Status == 0, 0, 1)

# Calculate the ROC curve and AUC score
roc_curve <- roc(pred_test, pred_train)
auc_score <- auc(roc_curve)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for Random Forest Model")
text(0.5, 0.3, paste("AUC =", round(auc_score, 3)))

ggroc(roc_curve, colour = 'steelblue', size = 2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_score, ')'))


#### Simulation ####
# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 1000  # Number of observations
p <- 9     # Number of predictors

# Generate predictors (X variables)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)

# Function to generate y with a specific proportion of class 1
generate_y <- function(X, prop_class1) {
  n <- nrow(X)
  prob <- 0.5 * X[, 1] - 0.3 * X[, 2] + rnorm(n)
  threshold <- quantile(prob, 1 - prop_class1)
  y <- ifelse(prob > threshold, 1, 0)
  return(y)
}

# Generate datasets
y_80 <- generate_y(X, 0.8)  # 80% class 1
y_50 <- generate_y(X, 0.5)  # 50% class 1

# Combine X and y into data frames
data_80 <- data.frame(X, y = y_80)
data_50 <- data.frame(X, y = y_50)

# Check the proportions of class 1 in each dataset
cat("Proportion of class 1 in 80% dataset:", mean(data_80$y), "\n")
cat("Proportion of class 1 in 50% dataset:", mean(data_50$y), "\n")

# View the datasets
head(data_80)
head(data_50)

data_50$y <- as.factor(data_50$y)
train_index_sim <- sample(nrow(data_50), 0.7 * nrow(data_50), replace = FALSE) 
train_data_sim <- data_50[train_index, ] 
test_data_sim <- data_50[-train_index, ] 

rf_sim <- randomForest(y ~ ., data = train_data_sim, ntree = 500, mtry= 2, importance = TRUE)
rf_sim

# Compute its predictions for training data
pred_train_sim <- predict(rf_sim, train_data_sim, type = "class")

# Display its predictions as confusion matrix for training data
table(pred_train_sim, train_data_sim$y)
# Calculating accuracy
mean(pred_train_sim == train_data_sim$y)


# Compute its predictions for test data
pred_test_sim <- predict(rf_sim, test_data_sim, type = "class")
# Display its predictions as confusion matrix for test data
table(pred_test_sim, test_data_sim$y)
# Calculating accuracy for test data
mean(pred_test_sim == test_data_sim$y)







# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 5000  # Number of observations
p <- 9     # Number of predictors

# Generate predictors (X variables)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)

# Function to generate y with specific correlation and proportion of class 1
generate_y <- function(X, prop_class1) {
  n <- nrow(X)
  # Create a linear combination of X variables with added noise
  prob <- 0.1 * X[, 1] + 0.001 * X[, 2] + 0.02 * X[, 3] + 0.03 * X[, 4] +
    0.04 * X[, 5]+ 0.04 * X[, 6]+0.01 * X[, 7]+0.06 * X[, 8]+0.04* X[, 9] + rnorm(n, mean = 0, sd = 0.5)
  # Determine the threshold for the desired class proportion
  threshold <- quantile(prob, 1 - prop_class1)
  # Assign classes based on the threshold
  y <- ifelse(prob > threshold, 1, 0)
  return(y)
}

# Generate datasets

y_50 <- generate_y(X, 0.5)  # 50% class 1

# Combine X and y into data frames

data_50 <- data.frame(X, y = y_50)

# Check the proportions of class 1 in each dataset

cat("Proportion of class 1 in 50% dataset:", mean(data_50$y), "\n")

# Ensure y is a factor
data_50$y <- as.factor(data_50$y)



# Split into training and test sets
train_index_50 <- sample(nrow(data_50), 0.8 * nrow(data_50), replace = FALSE)
train_data_50 <- data_50[train_index_50, ]
test_data_50 <- data_50[-train_index_50, ]


# Train Random Forest models
library(randomForest)
rf_50 <- randomForest(y ~ ., data = train_data_50, ntree = 100, mtry = 3, importance = TRUE, maxnodes = 10)


# Evaluate models
# Balanced dataset
pred_train_50 <- predict(rf_50, train_data_50, type = "class")
pred_test_50 <- predict(rf_50, test_data_50, type = "class")

cat("Accuracy on balanced train data:", mean(pred_train_50 == train_data_50$y), "\n")
cat("Accuracy on balanced test data:", mean(pred_test_50 == test_data_50$y), "\n")




