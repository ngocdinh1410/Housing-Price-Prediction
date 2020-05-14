###Housing Project#####

library(doMC)
library(dplyr)
library(purrr)
library(caret)
library(corrplot)
library(knitr)
library(ggplot2)
library(tidyverse)
library(car)
library(MASS)
library(pROC)
library(broom)
library(glmnet)
library(yardstick)

#Read the data
data <- read.csv("housing_data.csv",stringsAsFactors = FALSE)

#Check for missing data:
sum(is.na(data))
#No missing value to worry about!

#Exploratory Data Analysis:
summary(data)

#Histogram of salesprice:
ggplot(data, aes(x=SalePrice)) + 
  geom_histogram(color="darkblue", fill="lightblue",binwidth=10000)+
  geom_vline(aes(xintercept=median(SalePrice)),
             color="red", linetype="dashed", size=1)+
  labs(title="SalePrice Histogram",x="SalePrice", y = "Count")+
  scale_x_continuous(labels = scales::comma)

#Histogram of GrLivArea:
ggplot(data, aes(x=GrLivArea)) + 
  geom_histogram(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=median(GrLivArea)),
             color="red", linetype="dashed", size=1)+
  labs(x="Above ground living area square feet", y = "Count")+
  scale_x_continuous(labels = scales::comma)

#Histogram of LotArea:
ggplot(data, aes(x=LotArea)) + 
  geom_histogram(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=median(LotArea)),
             color="red", linetype="dashed", size=1)+
  labs(x="LotArea", y = "Count")+
  scale_x_continuous(labels = scales::comma)
# The red line is the mean, mean is about less than 200,000.

data$OverallQual <- as.factor(data$OverallQual)
p <- ggplot(data, aes(x=OverallQual, y=SalePrice, fill=OverallQual)) + 
  labs(title="Plot of Sale Price per Overall Quality",x="Overall Quality", y = "Sale Price")+
  scale_y_continuous(labels = scales::comma)+
  geom_boxplot()
p
#Higher quality higher sale Price, also more varaiance in sale price as quality is rated higher.

####
sapply(data, mode)
sapply(data, class)
#Correlation between numeric variables
num_data <- select_if(data, is.numeric)

#remove the ID column
num_data <- subset(num_data, select = -c(Id))

#correlation matrix of numeric variables
M <-cor(num_data)
head(round(M,2))
corrplot(M, method = "number", type="lower")


######################## Data Preprocess #############################################
# transform quality into numeric type
data <- transform(data, OverallQual = as.numeric(OverallQual))

# drop column ID
data$Id <- NULL

# create new building age column
data$YearRemodAdd <- (2020 - data$YearRemodAdd)

# create new building age column
data$Age <- (2020 - data$YearBuilt)
# drop column YearBuilt
data$YearBuilt <- NULL

# see the data after preprocess
summary(data)

#split data
# Set seed
set.seed(8765)

# create partition for train data
train_index <- createDataPartition(y = data$SalePrice, p = 0.7, list = FALSE)

# index train and test data
train <- data[train_index, ]
test <- data[-train_index, ]

# define function for calculating R-square
rsquare <- function(true, predicted) {
  sse <- sum((predicted - true)^2)
  sst <- sum((true - mean(true))^2)
  rsq <- 1 - sse / sst
  if (rsq < 0) rsq <- 0
  return (rsq)
}


######################## OLS Modelling 1 #############################################
#OSL model 1
model1 <- lm(SalePrice ~ ., data = train)

# predict on test set
pred.all1 <- predict(model1, test)
pred_train1 <- predict(model1, train)
model1_output <- data.frame(fit = c(pred.all1),
                           res = c(test$SalePrice - pred.all1))

#calculating rmse and r-square
ols1_RMSE_test<-RMSE(test$SalePrice,pred.all1)
ols1_RMSE_train<-RMSE(train$SalePrice,pred_train1)
ols1_rsquare_test<-rsquare(test$SalePrice, pred.all1)
ols1_rsquare_train<-rsquare(train$SalePrice, pred_train1)

#Linear regression assumptions
# 1. Normality?
augment(model1) %>%
  ggplot(aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line()
# 2. Constant variance?
# 3. Linearity?
augment(model1) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = .2) +
  geom_hline(yintercept = 0)
# 4. Independence of observations?
vif(model1)

# apply cross validation
dataset_glm_cv1 <- train(SalePrice ~ ., data = train, method = "glm",
                        trControl = trainControl(method = "repeatedcv",
                                                 repeats = 3, number = 5,
                                                 savePredictions = T))
dataset_glm_cv1$results
dataset_glm_cv1

# By calling resample, we can see how caret performed on each held out fold
dataset_glm_cv1$resample
cv1 <- dataset_glm_cv1$resample

#show the quantile of Rsquared
quantile(cv1$Rsquared)

# plot the rsquare distribution
ggplot(data = dataset_glm_cv1$resample, aes(x = Rsquared)) +
  geom_density(alpha = .2, fill="red")


######################## Improve training Data #########################################


# drop outliers of SalePrice below and above 1st and 99th percentile
outlierMinMax <- quantile(data$SalePrice, c(.99))
trainnew <- train[train$SalePrice < outlierMinMax[1], ]
  
# drop outliers of GrLivArea below and above 1st and 99th percentile
outlierMinMax <- quantile(data$GrLivArea, c(.99))
trainnew <- train[train$GrLivArea < outlierMinMax[1], ]

# drop outliers of LotArea below and above 1st and 99th percentile
outlierMinMax <- quantile(data$LotArea, c(.99))
trainnew <- trainnew[trainnew$LotArea < outlierMinMax[1], ]


######################## OLS Modelling 2 #############################################
#OSL model 2
model2<-lm(SalePrice ~ ., data=trainnew)

#predict on test set
pred.all2 <- predict(model2, test)
pred_train2 <- predict(model2, trainnew)
model2_output <- data.frame(fit = c(pred.all2),
                            res = c(test$SalePrice - pred.all2))

# calculating rmse and r-square
ols2_RMSE_test<-RMSE(test$SalePrice, pred.all2)
ols2_RMSE_train<-RMSE(trainnew$SalePrice, pred_train2)
ols2_rsquare_test <- rsquare(test$SalePrice, pred.all2)
ols2_rsquare_train <- rsquare(trainnew$SalePrice, pred_train2)

#Linear regression assumptions
# 1. Normality?
augment(model2) %>%
  ggplot(aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line()
# 2. Constant variance?
# 3. Linearity?
augment(model2) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = .2) +
  geom_hline(yintercept = 0)
# 4. Independence of observations?
vif(model2)

# apply cross validation
dataset_glm_cv2 <- train(SalePrice ~ ., data = trainnew, method = "glm",
                        trControl = trainControl(method = "repeatedcv",
                                                 repeats = 3, number = 5,
                                                 savePredictions = T))
dataset_glm_cv2$results
dataset_glm_cv2

# By calling resample, we can see how caret performed on each held out fold
dataset_glm_cv2$resample
cv2 <- dataset_glm_cv2$resample

#show the quantile of Rsquared
quantile(cv2$Rsquared)

# plot the rsquare distribution
ggplot(data = dataset_glm_cv2$resample, aes(x = Rsquared)) +
  geom_density(alpha = .2, fill="red")


######################## Ridge Modelling ###########################################
training_matrix <- model.matrix(SalePrice ~ .,data = trainnew)[, -1]
testing_matrix <- model.matrix(SalePrice ~ .,data = test)[, -1]

ridge <- cv.glmnet(x = training_matrix, y = trainnew$SalePrice, alpha = 0, nfolds = 5)
plot(ridge)
coef(ridge, s = "lambda.min")
coef(ridge, s = "lambda.1se")

# calculate R-square and rmse
ridge_train_output <- predict(ridge, s = ridge$lambda.1se, 
                               newx = training_matrix)
ridge_rsquare_train <- rsquare(trainnew$SalePrice, ridge_train_output)
ridge_testing_predictions <- predict(ridge, s = ridge$lambda.1se,
                                     newx = testing_matrix)
ridge_output <- data.frame(fit = c(ridge_testing_predictions),
                           res = c(test$SalePrice - ridge_testing_predictions))
ridge_rsquare_test <- rsquare(test$SalePrice, ridge_testing_predictions)
ridge_RMSE_train <- RMSE(trainnew$SalePrice,ridge_train_output)
ridge_RMSE_test <- RMSE(test$SalePrice,ridge_testing_predictions)

#Linear regression assumptions
# 1. Normality?
ridge_output %>%
  ggplot(aes(sample = res)) +
  geom_qq() +
  geom_qq_line()
# 2. Constant variance?
# 3. Linearity?
ridge_output %>%
  ggplot(aes(x = fit, y = res)) +
  geom_point() +
  geom_hline(yintercept = 0)
# 4. Independence of observations?



######################## Lasso Modelling ###########################################
lasso <- cv.glmnet(x = training_matrix, y = trainnew$SalePrice, alpha = 1, nfolds = 5)
plot(lasso)
coef(lasso, s = "lambda.min")
coef(lasso, s = "lambda.1se")

# calculate R-square and rmse
lasso_train_output <- predict(lasso, s = lasso$lambda.1se, 
                              newx = training_matrix)
lasso_rsquare_train <- rsquare(trainnew$SalePrice, lasso_train_output)
lasso_testing_predictions <- predict(lasso, s = lasso$lambda.1se,
                                     newx = testing_matrix)
lasso_output <- data.frame(fit = c(lasso_testing_predictions),
                           res = c(test$SalePrice - lasso_testing_predictions))
lasso_rsquare_test <- rsquare(test$SalePrice, lasso_testing_predictions)
lasso_RMSE_train <- RMSE(trainnew$SalePrice,lasso_train_output)
lasso_RMSE_test <- RMSE(test$SalePrice,lasso_testing_predictions)

#Linear regression assumptions
# 1. Normality?
lasso_output %>%
  ggplot(aes(sample = res)) +
  geom_qq() +
  geom_qq_line()
# 2. Constant variance?
# 3. Linearity?
lasso_output %>%
  ggplot(aes(x = fit, y = res)) +
  geom_point() +
  geom_hline(yintercept = 0)
# 4. Independence of observations?



######################## Model Evaluation & Comparison ###############################
# 1)
#OLS model 1 - summary
summary(model1)
# OLS model 1 - RMSE and R-square
data.frame(ols1_RMSE_train, ols1_RMSE_test, ols1_rsquare_train, ols1_rsquare_test)
# OLS model 1 - CV
dataset_glm_cv1$results

# 2) 
# OLS model 2 (removed variables) - summary
summary(model2)
# OLS model 2 (removed variables) - RMSE and R-square
data.frame(ols2_RMSE_train, ols2_RMSE_test, ols2_rsquare_train, ols2_rsquare_test)
# OLS model 1 - CV
dataset_glm_cv2$results

# 3)
# Ridge Model - RMSE and R-square
data.frame(ridge_RMSE_train, ridge_RMSE_test, ridge_rsquare_train, ridge_rsquare_test)

# 4)
# Lasso Model - RMSE and R-square
data.frame(lasso_RMSE_train, lasso_RMSE_test, lasso_rsquare_train, lasso_rsquare_test)




