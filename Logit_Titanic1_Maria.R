
################################# Logistic Regression Model ###############################

# Predict passengers r survived or not in titanic

################################# Logistic Regression Model ###############################
# Compute confidance Interval
# Perform Chie Squared Test
# Analysis of Deviance table
# Calculate AUC 
# & try to Improve model performance 
###########################################################################################

# Step 1: Load Data / read data
# -------------------------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Logistic Regression\\Titanic_Dataset")

titanic_train = read.csv('train.csv',  header = T, na.strings = c(''))
str(titanic_train)

test_label = read.csv('gender_submission.csv', header = T, na.string=c(""))
str(test_label)

titanic_test = read.csv('test.csv', header = T, na.string=c(""))
str(titanic_test)

titanic_test = merge(titanic_test, test_label, by = "PassengerId")
str(titanic_test)

# Step 2: Analyse Dataset
# ----------------------------------------------------------------------------------------

# Convert Catogorical variables into factor
titanic_train$Pclass = as.factor(titanic_train$Pclass)
titanic_test$Pclass = as.factor(titanic_test$Pclass)
str(titanic_train)
str(titanic_test)

titanic_train[which(titanic_train$Age < 1),'Age']
titanic_test[which(titanic_test$Age < 1),'Age']

# Find Missing value columns
sapply(titanic_train, function(x) sum(is.na(x)))
sapply(titanic_test, function(x) sum(is.na(x)))

# Calculate unique vales for each variable
sapply(titanic_train, function(x) length(unique(x)))
sapply(titanic_test, function(x) length(unique(x)))

# Visual representation of missing values
library(Amelia) 
missmap(titanic_train, main = "Missing Values vs. Observed")
missmap(titanic_test, main = "Missing Values vs. Observed")

titanic_train = subset(titanic_train, select = c(2,3,5,6,7,8,10,12))
titanic_test = subset(titanic_test, select = c(2,4,5,6,7,9,11,12))

# Missing value Imputation
age = c(titanic_train$Age, titanic_test$Age)
avg_age = mean(age, na.rm = T)
titanic_train$Age[is.na(titanic_train$Age)] = avg_age
titanic_test$Age[is.na(titanic_test$Age)] = avg_age

titanic_train = titanic_train[!is.na(titanic_train$Embarked),]
titanic_test = titanic_test[!is.na(titanic_test$Fare),]

missmap(titanic_train, main = "Missing Values vs. Observed")
missmap(titanic_test, main = "Missing Values vs. Observed")

# Step 3: Implement Logit model on training data
# ----------------------------------------------------------------------------------------

reg_model = glm(Survived ~., family = binomial(link = 'logit'), data = titanic_train)
summary(reg_model)

# Compute confidence interval for one or more parameter in fitted model
confint(reg_model)

# Perform Chi - squared Test
library(aod)
wald.test(b = coef(reg_model), Sigma = vcov(reg_model), Terms = 9:10)
wald.test(b = coef(reg_model), Sigma = vcov(reg_model), Terms = 2:3)

exp(cbind(OR = coef(reg_model), confint(reg_model)))

anova(reg_model, test = 'Chisq')

library(pscl)
pR2(reg_model)

# Step 4: Model Performance Evaluation (Model Prediction)
# ----------------------------------------------------------------------------------------

titanic_predict = predict(reg_model, newdata = subset(titanic_test, select = c(1:7)),
                          type = 'response')
head(titanic_predict)

titanic_predict = ifelse(titanic_predict >0.5, 1, 0)
head(titanic_predict)

# Step 5: Check Model Accuracy
# ----------------------------------------------------------------------------------------

misClassifiError = mean(titanic_predict != titanic_test$Survived)
print(paste('Accuracy', 1 - misClassifiError))

# Step 6: Draw ROC Curve
# ----------------------------------------------------------------------------------------

library(ROCR)
p = predict(reg_model, newdata = subset(titanic_test, select = c(1:7)),
            type = 'response')
pre = prediction(p, titanic_test$Survived)
perf = performance(pre, measure = 'tpr', x.measure = 'fpr')
plot(perf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 01))
# OR
# plot(perf)

abline(0,1, lwd = 2, lty = 2)

# Calculate AUC
auc = performance(pre, measure = 'auc')
str(auc)
auc = auc@y.values[[1]]
auc

# Step 7: Model Performance Improvement
# ----------------------------------------------------------------------------------------

library(kernlab)
svm_model <- ksvm(Survived ~ ., data = titanic_train, kernel = 'vanilladot')
svm_model

svm_predict <- predict(svm_model, titanic_test[,1:7])
head(svm_predict)

svm_predict = ifelse(svm_predict >0.5, 1, 0)
head(svm_predict)

table(svm_predict, titanic_test$Survived)

# Confusion Matrix
misClassifiError = mean(svm_predict != titanic_test$Survived)
print(paste('Accuracy is', 1 - misClassifiError))

p = predict(svm_model, newdata = subset(titanic_test, select = c(1:7)),
            type = 'response')
pre = prediction(p, titanic_test$Survived)
perf = performance(pre, measure = 'tpr', x.measure = 'fpr')
plot(perf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 01))
# OR
#plot(perf)
abline(0,1, lwd = 2, lty = 2)
# OR

auc = performance(pre, measure = 'auc')
str(auc)
auc = auc@y.values[[1]]
auc

######################################## END ###################################################

