
################################### Classificaation Model #########################################

# Using Predictive Models to Classify Pima Indians Diabetes Database

################################### Classificaation Model #########################################

## ------------------------------Load Libraries-----------------------------------------------
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(party)
library(RColorBrewer)
library(ROCR)
library(class)
library(rpart)
library(rattle)
library(rpart.plot)


setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Classification_Comparitive Model\\DibetecDB")
pima_Logit <- read.csv("diabetes.csv")

str(pima_Logit)
head(pima_Logit)

colSums(is.na(pima_Logit)) # Check for missinng values

# Produce matrix of scatter plot
pairs(pima_Logit, panel = panel.smooth)

# Compute matrix of correlations between variables
corrplot(cor(pima_Logit[, -9]), type = "lower", method = "number")

# Preparing the DataSet(Spliting)

set.seed(123)
n <- nrow(pima_Logit)
train <- sample(n, trunc(0.70*n))
pima_training <- pima_Logit[train, ]
pima_testing <- pima_Logit[-train, ]

dim(pima_training); dim(pima_testing)

################ Apply Logistic Regression ##########################

# Training The Model

glm_fm1 <- glm(Type ~., data = pima_training, family = binomial)
summary(glm_fm1)

## Remove non significat variables
glm_fm2 <- update(glm_fm1, ~. - Skin - Ins - Age )
summary(glm_fm2)

## Plot the new Model
par(mfrow = c(2,2))
plot # 
# 1. Residual Vs Fitted values: 
  ## our residuals have logaritmic pattern that means we got a better model.
# 2. Normal Q-Q : 
  ## check if our residuals follow Normal distribution or not; The residuals are normally distributed if the points follow the dotted line closely;
# 3. Scale-Location :  
  ## indicates spread of points across predicted values range. One of the assumptions for 
  ## Regression is Homoscedasticity . i.e variance should be reasonably
  ## equal across the predictor range; . A horizontal red line is ideal and would indicate that 
  ## residuals have uniform variance across the range. As residuals spread wider from each other the red spread line goes up;
  ## In this case the data is Homoscedastic i.e has uniform variance.
# 4. Residuals vs Leverage Plot: 
  ## Influence : The Influence of an observation can 
  ## be thought of in terms of how much the predicted scores would change if the observation is excluded.
  ## Leverage : The leverage of an observation is based on how much the observation's value 
  ## on the predictor variable differs from the mean of the predictor variable. The more the 
  ## leverage of an observation , the greater potential that point has in terms of influence.

# Apply the model to the testing sample

glm_probs <- predict(glm_fm2, newdata = pima_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)

# Confusion Matrix for logistic regression

cnm_Logit <- table(Predicted = glm_pred, Actual = pima_testing$Type) 
cn <- confusionMatrix(cnm_Logit) ;cn
acc_glm <- cn$overall['Accuracy']; acc_glm


######################### Implement Decision Tree ###################################

# Preparing the DataSet:

pima_DT <- read.csv("diabetes.csv")
pima_DT$Type <- as.factor(pima_DT$Type)

library(caret)
library(tree)
library(e1071)

# Spliting Dataset

set.seed(1000)
intrain <- createDataPartition(y = pima_DT$Type, p = 0.7, list = FALSE)
train_DT <- pima_DT[intrain, ]
test_DT <- pima_DT[-intrain, ]

dim(train_DT); dim(test_DT)

# Training The Model

treemod <- tree(Type ~ ., data = train_DT)
summary(treemod)

treemod # get a detailed text output.

# Plot the tree
plot(treemod)
text(treemod, pretty = 0)

# Testing the Model
tree_pred <- predict(treemod, newdata = test_DT, type = "class" )
confusionMatrix(tree_pred, test_DT$Type)

acc_treemod <- confusionMatrix(tree_pred, test_DT$Type)$overall['Accuracy']
acc_treemod

######################## Implement Random forest model ################################

# Training The Model

set.seed(123)
library(randomForest)

rf_pima <- randomForest(Type ~., data = pima_training, mtry = 8, ntree=50, importance = TRUE)
rf_pima

# Testing the Model

rf_probs <- predict(rf_pima, newdata = pima_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)

cn_rf <- table(Predicted = rf_pred, Actual = pima_testing$Type); cn_rf
acc_cn_rf <- confusionMatrix(cn_rf); acc_cn_rf
acc_rf_pima <- acc_cn_rf$overall['Accuracy']; acc_rf_pima

# The important variable
importance(rf_pima)  # Result shows: Glucose (Glu) is most IMP variable

# Plot the variable importance
par(mfrow = c(1, 2))
varImpPlot(rf_pima, type = 2, main = "Variable Importance",col = 'black')
plot(rf_pima, main = "Error vs no. of trees grown")


########################### Implement Support Vector Machine Model ###########################

#pima_svm <- read.csv("diabetes.csv")
#pima_svm$Type <- as.factor(pima_svm$Type)

#library(e1071)

#Preparing the DataSet:
#set.seed(1000)
#intrain <- createDataPartition(y = pima_svm$Type, p = 0.7, list = FALSE)
#train_svm <- pima[intrain, ]
#test_svm <- pima[-intrain, ]

#dim(train_svm); dim(test_svm)

# Implement Model

#tuned <- tune.svm(Type ~., data = train_svm, gamma = 10^(-6:-1), cost = 10^(-1:1))
#summary(tuned) # to show the results

# Train the model
#svm_model  <- svm(Type ~., data = train_svm, kernel = "radial", gamma = 0.01, cost = 10) 
#summary(svm_model)


# Test the Model
#svm_pred <- predict(svm_model, newdata = test_svm)

#cn_svm <- table(Predicted = svm_pred, Actual = test_svm$Type); cn_svm
#acc_cn_svm <- confusionMatrix(cn_svm); acc_cn_svm
#acc_svm_pima <- acc_cn_svm$overall['Accuracy']; acc_svm_pima


############# Comparison of Model Accuracy ################################

accuracy <- data.frame(Model=c("Logistic Regression","Decision Tree","Random Forest"), 
                       Accuracy=c(acc_glm, acc_treemod, acc_rf_pima))
ggplot(accuracy,aes(x=Model,y=Accuracy)) + 
  geom_bar(stat='identity') + theme_bw() + 
  ggtitle('Comparison of Model Accuracy')


############################################## END ############################################