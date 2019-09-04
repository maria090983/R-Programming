
############################### Logistic Regression Project ##################################

# Predict whether Patient is diabetic or Not

##############################################################################################

# Step 1: Read Data/ Import Data
# ---------------------------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Logistic Regression\\DibetecDB")
data <- read.csv("diabetes.csv")
View(data)
str(data)

# Step 2: Divide Data (Train & Test)
#----------------------------------------------------------------------------------------------

library(caTools)

split_data <- sample.split(data, SplitRatio = 0.8)
split_data

train_data <- subset(data, split_data == "TRUE")
test_data <- subset(data, split_data == "FALSE")

# Step 3: Implement Model using Training dataset
#----------------------------------------------------------------------------------------------

attach(data)
Logit_model <- glm(Type~., train_data, family = "binomial")
summary(Logit_model)

# Step 4 : Optimize Model
#----------------------------------------------------------------------------------------------

opt_model <- glm(Type~ Npreg + Glu + BP + Ins + BMI + PedF + Age, train_data, family = "binomial")
summary(opt_model)

opt_model2 <- glm(Type~ Npreg + Glu + BP + BMI + PedF, train_data, family = "binomial")
summary(opt_model2)

# Step 5: Calculate probability Of Y  (For Test & Train) (For Test & Train)
#----------------------------------------------------------------------------------------------

train_res <- predict(opt_model2, train_data, type = "response")
train_res

test_res <- predict(opt_model2, test_data, type = "response")
test_res

# Step 7: Creating Confusion Matrix and calculate accuracy
#----------------------------------------------------------------------------------------------

cn_matrix_train_data <- (table(ActualValue = train_data$Type, PredictedValue = train_res > 0.5))
cn_matrix_train_data
cn_matrix_test_data <- (table(ActualValue = test_data$Type, PredictedValue = test_res > 0.5))
cn_matrix_test_data

#confusionMatrix(as.factor(test_res),test_data$Type, positive="1")

# Step 8: To minimize type error, we have to change threshold value
# For this calculate ROC curve
#----------------------------------------------------------------------------------------------

library(ROCR)

rocr_pre <- prediction(train_res, train_data$Type)
rocr_perf <- performance(rocr_pre, "tpr", "fpr")

plot(rocr_perf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 01))

cn_matrix_train_data <- (table(ActualValue = train_data$Type, PredictedValue = train_res > 0.3))
cn_matrix_train_data
cn_matrix_test_data <- (table(ActualValue = test_data$Type, PredictedValue = test_res > 0.3))
cn_matrix_test_data

######################################## END ###################################################

