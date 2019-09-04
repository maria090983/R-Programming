

############################# Logistic Regression Project ######################################

# Predict whether Car have V-Shaped Engine(VS = 0) Or Straight Engine(V = 1) -> Use in built data of MtCars

###############################################################################################
# Modified Threshold and check the performance of model
###############################################################################################

# Step 1: Read Data/ Import Data
# ---------------------------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Logistic Regression\\MtCarsDB")
df_mtcars <- read.csv("mtcars.csv")
View(df_mtcars)
head(mtcars)
str(mtcars)

# Step 2: Divide Data (Train & Test)
#----------------------------------------------------------------------------------------------

library(caTools)

split_data <- sample.split(df_mtcars, SplitRatio = 0.8)
split_data

train_data <- subset(df_mtcars, split_data == "TRUE")
test_data <- subset(df_mtcars, split_data == "FALSE")

# Step 3: Implement Model using Training dataset
#----------------------------------------------------------------------------------------------

attach(df_mtcars)
Logit_model <- glm(vs~., train_data, family = "binomial")
summary(Logit_model)

# Step 4 : Optimize Model
#----------------------------------------------------------------------------------------------

opt_model <- glm(vs~ disp + wt, train_data, family = "binomial")
summary(opt_model)

# Step 5: Calculate probability Of Y  (For Test & Train)
#----------------------------------------------------------------------------------------------

train_res <- predict(opt_model, train_data, type = "response")
train_res

test_res <- predict(opt_model, test_data, type = "response")
test_res

# Step 7: Creating Confusion Matrix
#----------------------------------------------------------------------------------------------

cn_matrix_train_data <- (table(ActualValue = train_data$vs, PredictedValue = train_res > 0.5))
cn_matrix_train_data
cn_matrix_test_data <- (table(ActualValue = test_data$vs, PredictedValue = test_res > 0.5))
cn_matrix_test_data

# Check Accuracy
#library(caret)
#confusionMatrix(as.factor(test_res),test_data$vs, positive="1") # data` and `reference` should be factors with the same levels

# Step 8: To minimize type error, we have to change threshold value
# For this calculate ROC curve
#----------------------------------------------------------------------------------------------

library(ROCR)
library(e1071)
rocr_pre <- prediction(train_res, train_data$vs)
rocr_perf <- performance(rocr_pre, "tpr", "fpr")

plot(rocr_perf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 01))

# Modified threshold value
cn_matrix_train_data <- (table(ActualValue = train_data$vs, PredictedValue = train_res > 0.3))
cn_matrix_train_data
cn_matrix_test_data <- (table(ActualValue = test_data$vs, PredictedValue = test_res > 0.3))
cn_matrix_test_data
# After changing threshold same result

########################################### END ##############################################

