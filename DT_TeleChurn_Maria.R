################################### Decision Treee #############################################

# Build classification models using decision tree algorithm to predict whether the customer 
# be churned or not on the basis of its billing information and customer demographics

################################### Decision Treee #############################################


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

# Step 1: Load Data
## ------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Decision Tree\\ChurnDB")
df_churn <- read.csv('Churn.csv')
head(df_churn)
str(df_churn)

# Step 2: Data Preprocessing / Analysing
## ------------------------------------------------------------------------

## ------------Missing value verification & Imputation----------------------
colSums(is.na(df_churn))
# OR
sapply(df_churn, function(x) sum(is.na(x)))
# No Missing Values

## -----------------Check Class Is Baised or not---------------------------------
count(df_churn$gender)  # Count() included in library(plyr)
# OR
count(df_churn, 'gender')
# Class not Biased

## ------------------------------------------------------------------------
count(df_churn, 'SeniorCitizen')
count(df_churn, ' Partner')
count(df_churn, 'Dependents')
count(df_churn, 'tenure')
count(df_churn, ' CallService')
count(df_churn, ' MultipleConnections')
count(df_churn, ' InternetConnection')
count(df_churn, ' OnlineSecurity')
count(df_churn, ' OnlineBackup')
count(df_churn, ' DeviceProtectionService')
count(df_churn, ' TechnicalHelp')
count(df_churn, ' OnlineTV')
count(df_churn, ' OnlineMovies')
count(df_churn, ' Agreement')
count(df_churn, ' BillingMethod')
count(df_churn, ' PaymentMethod')
count(df_churn, ' MonthlyServiceCharges')
count(df_churn, ' TotalAmount')
count(df_churn, ' Churn')

## ----------------Map Values( Replace values with levels)--------------------
cols_name <- c(10:15)    # only for 10 to 15 columns
for(i in 1:ncol(df_churn[,cols_name])) 
    {
        df_churn[,cols_name][,i] <- as.factor(mapvalues (df_churn[,cols_name][,i], 
                                                         from =c("No internet service"),
                                                         to=c("No")))  # mapValues() inluded in library(plyr)
    }
## ------------------------------------------------------------------------
df_churn$MultipleConnections <- as.factor(mapvalues(df_churn$MultipleConnections, 
                                           from=c("No phone service"),
                                           to=c("No")))
## ------------------------------------------------------------------------
df_churn$SeniorCitizen <- as.factor(mapvalues(df_churn$SeniorCitizen,
                                      from=c("0","1"),
                                      to=c("No", "Yes")))

## ----------------Remove CustID(not IMP So)---------------------------------
df_churn$customerID <- NULL

## ------------Find Numerical variables and plot Correlation matrix-----------
numeric_var <- sapply(df_churn, is.numeric) ## Find numerical variables
corr_matrix <- cor(df_churn[,numeric_var])  ## Calculate the correlation matrix
corrplot(corr_matrix, main="\n\nCorrelation Plot for Numeric Variables", method="number")

## ----------------Bar Plot For (Gender/Partner/Dependent)-----------------------
plot1 <- ggplot(df_churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot2 <- ggplot(df_churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot3 <- ggplot(df_churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(plot1, plot2, plot3, ncol=2)

## ------(Call Service/Multiple Conn/Internet Conn/Online Security----------
plot4 <- ggplot(df_churn, aes(x=CallService)) + ggtitle("Call Service") + xlab("Call Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot5 <- ggplot(df_churn, aes(x=MultipleConnections)) + ggtitle("Multiple Connections") + xlab("Multiple Connections") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot6 <- ggplot(df_churn, aes(x=InternetConnection)) + ggtitle("Internet Connection") + xlab("Internet Connection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot7 <- ggplot(df_churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(plot4, plot5, plot6, plot7, ncol=2)

## ------------(online Movie/Agreement/Billing Method/Payment)-------------
plot12 <- ggplot(df_churn, aes(x=OnlineMovies)) + ggtitle("Online Movies") + xlab("Online Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot13 <- ggplot(df_churn, aes(x=Agreement)) + ggtitle("Agreement") + xlab("Agreement") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot14 <- ggplot(df_churn, aes(x=BillingMethod)) + ggtitle("Billing Method") + xlab("Billing Method") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot15 <- ggplot(df_churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(plot12, plot13, plot14, plot15,  ncol=2)

## --------------------Remove Tenure/PayMethod/gender----------------------
df_churn$tenure <- NULL
df_churn$PaymentMethod <- NULL
df_churn$gender <- NULL

# Step 3: Divide data(Test & Train)
## ------------------------------------------------------------------------

intrain <- createDataPartition(df_churn$Churn,p=0.7,list=FALSE)
set.seed(2000)
training <- df_churn[intrain,]
testing <- df_churn[-intrain,]

nrow(training)
nrow(testing)
# OR 
dim(training); dim(testing)

# Step 4: Create Model
## ------------------------------------------------------------------------

model_tree <- rpart(Churn ~ ., training, method = "class", control = list(maxdepth = 6))
model_tree
summary(model_tree)

## ------------------Visual Representation of model-----------------------
fancyRpartPlot(model_tree)

## ----------------------  display the results----------------------------
printcp(model_tree)  # Cp - Complexity parameter

## ------------------------------------------------------------------------
plotcp(model_tree) # visualize cross-validation results 

# Step 6: Model Prediction (on Test & Train Data)
## ------------------------------------------------------------------------

pred_training <- predict(model_tree, training,type = "class")   # Training Data

## ----------------Construct the confusion matrix: conf--------------------
conf_matrix <- table(Actual_Train = training$Churn, Predicted_Train = pred_training) 
conf_matrix       
# Accuracy = 0.744(74%)

## ------------------------------------------------------------------------
pred_test <- predict(model_tree, testing, type = "class")   # Test Data

## ----------------Construct the confusion matrix: conf--------------------
conf_matrix <- table(Actual_Test = testing$Churn, Predicted_Test = pred_test)
conf_matrix
# Accuracy = 0.729(73%)

## ----------------------Print out the accuracy----------------------------
Accuracy = sum( diag(conf_matrix) ) / sum(conf_matrix)
Accuracy

# Step 7: Prune the tree: pruned & Predict the Test Model
## ------------------------------------------------------------------------
#Take least cp where xvalidation error is less to prune
pruned <- prune(model_tree, cp = 0.02)
pruned

## ---------------------------Draw pruned----------------------------------
fancyRpartPlot(pruned)

## ------------------------------------------------------------------------
pred_pruned <- predict(pruned, testing, type = "class")  # Predict pruned

## ------------------------------------------------------------------------
conf_i <- table(testing$Churn, pred_pruned)   #Confusion Matrix
conf_i

## ------------------------------------------------------------------------
Purn_Accuracy <- sum( diag(conf_i) ) / sum(conf_i) # Accuracy = 0.707 (71%)
Purn_Accuracy

# Step 8: Revise Model/ Model Optimization
## ------------------------------------------------------------------------

# Change the first line of code to use information gain as splitting criterion
model_i <- rpart(Churn ~ ., training, method = "class",
                parms = list(split = "information"),
                control = rpart.control(cp = 0, maxdepth = 6,minsplit = 100))

## ------------------------------------------------------------------------
printcp(model_i) # display the results 

## ------------------------------------------------------------------------
plotcp(model_i) # visualize cross-validation results 

## ------------------------------------------------------------------------
pred_i <- predict(model_i, testing, type = "class") # Predict Test Model

## ------------------------------------------------------------------------
conf_i <- table(testing$Churn, pred_i) # Confusion Matrix
conf_i

## ------------------------------------------------------------------------
Accuracy_i <- sum( diag(conf_i) ) / sum(conf_i)  # Accuracy = 0.734(73%) Little bit improved
Accuracy_i

## --------------------------Pruning-----------------------------------
# Prune the tree: pruned (Change the CP)
pruned_i <- prune(model_i, cp = 0.01)

## ------------------------------------------------------------------------
# Draw pruned
fancyRpartPlot(pruned_i)

## ------------------------------------------------------------------------
pred_pruned <- predict(pruned_i, testing, type = "class") # Predict pruned model

## ------------------------------------------------------------------------
prun_confu <- confusionMatrix(testing$Churn, pred_pruned) # confusion matrix
prun_confu  # Accuracy = 0.707(71%)

## ------------------------------------------------------------------------
all_probs <- predict(pruned_i, testing, type = "prob") # Predict test model/calculate Probability

## ------------------------------------------------------------------------
probs <- all_probs[, 2]
probs

## ------------------------------------------------------------------------
# Make a prediction object: pred
pred_test <- prediction(probs, testing$Churn)

# Make a performance object: perf
perf <- performance(pred_test, "tpr", "fpr")
plot(perf , col="blue")
abline(a=0,b=1)

######################################## END ################################################
