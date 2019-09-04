

## ----------Logistic Regression Project 2 - Bank Credit Card  Default Prediction ------------

#Build a classification model using logistic regression to predict the credibility of the customer, 
#in order to minimize the risk and maximize the profit of German Credit Bank

# Step 1: Read File/ Import File
#--------------------------------------------------------------------------------------------
setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Logistic Regression\\CreditCardDB")
df_bank = read.csv("BankCreditCard.csv")

# Step 2: Analyze Dtaset
#--------------------------------------------------------------------------------------------

# check the number of rows and columns
nrow(df_bank)
ncol(df_bank)
View(df_bank)
colnames(df_bank)

# describing the dataset
str(df_bank)

# Step 3: Check Class is Bias OR Not
#--------------------------------------------------------------------------------------------

table(df_bank$Default_Payment)

# Clearly, there is a class bias, a condition observed when the proportion of events occure(1) 
# is much smaller than proportion of event not occure(0). So we must sample the observations 
# in approximately equal proportions to get better models

# Converting columns to factors according to the data description 
cols_to_factors <- c("Gender", "Academic_Qualification","Marital", "Repayment_Status_Jan",
                     "Repayment_Status_Feb","Repayment_Status_March","Repayment_Status_April",
                     "Repayment_Status_May","Repayment_Status_June", "Default_Payment")
df_bank[cols_to_factors] <- lapply(df_bank[cols_to_factors], factor)
str(df_bank)

# -------------------------------------------------------------------------------
# apply() 1--> row, 2 --> columns
# check for Nulls for all columns

# Step 4: Check for missing values and blanks
#--------------------------------------------------------------------------------------------

colSums(is.na(df_bank))
# OR
col_name <- colnames(df_bank) [apply(df_bank, 2, function(n) any(is.na(n)))]
if(length(col_name) > 0) print("NULLs present") else print("No NULLs")

# Check for Blanks for all columns
col_name = colnames(df_bank) [apply(df_bank, 2, function(n) any(n == " "))]
if(length(col_name) > 0) print("Blanks present") else print("No Blanks")

# Step 5: Rename Categorical Variables
#--------------------------------------------------------------------------------------------

# ------- Renaming Male/Female levels for Gender variable --------------

levels(df_bank$Gender)[levels(df_bank$Gender) == "1"] <- "Male"
levels(df_bank$Gender)[levels(df_bank$Gender) == "2"] <- "Female"

str(df_bank$Gender)

# ------------- Renaming levels for Marital variable ------------------

levels(df_bank$Marital)[levels(df_bank$Marital) == "1"] <- "Married"
levels(df_bank$Marital)[levels(df_bank$Marital) == "2"] <- "Single"
levels(df_bank$Marital)[levels(df_bank$Marital) == "3"] <- "Do not say"

str(df_bank$Marital)

# -------- Renaming levels for Academic Qualification -----------------

levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "1"] <- "Undergraduate"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "2"] <- "Graduate"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "3"] <- "Postgraduate"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "4"] <- "Professional"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "5"] <- "Others"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "6"] <- "Unknown"

str(df_bank$Academic_Qualification)

# Step 6: Randomly shuffling the dataset
#--------------------------------------------------------------------------------------------

grp = runif(nrow(df_bank))
df_bank = df_bank[order(grp),]

# Step 7: Divide Data set (Train & Test)
#--------------------------------------------------------------------------------------------

library(caret)
train_rows<- createDataPartition(y= df_bank$Default_Payment, p=0.7, list = FALSE)
train_data<- df_bank[train_rows,] # 70% data goes in here
table(train_data$Default_Payment)

test_rows<-  createDataPartition(y= df_bank$Default_Payment, p=0.3, list = FALSE)
test_data<- df_bank[test_rows,] # 30% data goes in here
table(test_data$Default_Payment)

# number of records in train and test datasets
nrow(train_data)
nrow(test_data)

# Step 8: Build Model & Summarize it
#--------------------------------------------------------------------------------------------
# GLM - generalised linear model

glm_full_model = glm(Default_Payment ~ ., family = "binomial", data=train_data)   #we do binomial(0 or 1) classification
summary(glm_full_model)    # summarise model

# Step 9: Predicting probabilities obtain from the model
#--------------------------------------------------------------------------------------------

# predict the Y-values
predict_full_model = predict(glm_full_model,test_data,type="response")  #response means it gives probabilities
pred_full_model

head(predict_full_model)
table(train.data$Default_Payment)

# Step 10: Build the confusion matrix
#--------------------------------------------------------------------------------------------
#install.packages("e1071")
library(e1071)

(table(ActualValue = test_data$Default_Payment, PredictedValue = predict_full_model > 0.5))
(table(PredictedValue = predict_full_model > 0.5, ActualValue = test_data$Default_Payment))

# ----------------------- OR ---------------------------------------

predictions_full_model = ifelse(predict_full_model <=0.5, 0, 1) 
predictions_full_model   # 0 -> means Event Not Occuring: 1 -> Event Occuring
table(Predicted = predictions_full_model,Actual = test_data$Default_Payment)

# Check Accuracy
confusionMatrix(as.factor(predictions_full_model),test_data$Default_Payment, 
                positive="1")   # Confusion matrix function included in caret package

## ------------------------------------------------------------------------

# In the above result, we can see that TP = 673 & TN = 6672 which are correct prediction. 
# and FP = 1318, FN = 338 which are incorrect predictions
# Here we got 34% sensitivity i.e. observation of positive class are labelled correctly
# 95% specificity i.e. observation of negative class are labelled correctly
# 82% accuracy i.e. all correct predictions

# Step 11: Build ROC Curve
## ------------------------------------------------------------------------

library("ROCR")
#install.packages("plotROC")
#library(plotROC)
pred_full_model <- prediction(predict(glm_full_model),train_data$Default_Payment)
perf_full_model <- performance(pred_full_model,"tpr","fpr")
plot(perf_full_model)
# OR
plot(perf_full_model, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 01))

# Step 12: Optimize Model
# ------------------------------------------------------------------------
# Build the logistic regression model using significant variables
# GLM - generalised linear model
glm_Sig = glm(Default_Payment ~ Credit_Amount+Gender+Marital+Repayment_Status_Jan+
                Repayment_Status_Feb+Repayment_Status_March+Repayment_Status_April+
                Jan_Bill_Amount+Feb_Bill_Amount+March_Bill_Amount+Previous_Payment_Jan+
                Previous_Payment_Feb+Previous_Payment_April+Previous_Payment_May, 
              family = "binomial", data=train_data) #we do binomial classification

# summarise model
summary(glm_Sig)

# predict the Y-values
predict_Sig = predict(glm_Sig,test_data,type="response") #response means it gives prob

head(predict_Sig)

table(train_data$Default_Payment)

predictions_Sig = ifelse(predict_Sig <=0.5, 0,1) 

# build the confusion matrix
table(predicted = predictions_Sig,actual = test_data$Default_Payment)
confusionMatrix(as.factor(predictions_Sig),test_data$Default_Payment, 
                positive="1")

# plot Roc Curve for above model

pred_sig <- prediction(predict(glm_Sig),train_data$Default_Payment)
perf_sig <- performance(pred_sig,"tpr","fpr")  # tpr/fpr -> True/FALSE Positive Rate
plot(perf_sig)
# OR
plot(perf_sig, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 01))

## ------------------------------------------------------------------------

# Here both models have approximately same accuracy so we can use second model 
# according to occam's razor principle

# ------------------------------------##### END ####---------------------------------------

# ----------------------------------- Modedl 1--------------------------------------------

glm_full_model_1 = glm(Default_Payment ~ Credit_Amount, family = "binomial", data=train_data) 
summary(glm_full_model_1)

predict_full_model_1 = predict(glm_full_model_1,test_data,type="response") 

head(predict_full_model_1)
table(train_data$Default_Payment)

predictions_full_model_1 = ifelse(predict_full_model_1 <=0.5, 0, 1) 

table(predicted = predictions_full_model_1,actual = test_data$Default_Payment)
confusionMatrix(as.factor(predictions_full_model_1),test_data$Default_Payment, 
                positive="1")

pred_full_model_1 <- prediction(predict(glm_full_model_1),train_data$Default_Payment)
perf_full_model_1 <- performance(pred_full_model_1,"tpr","fpr")
plot(perf_full_model_1)

glm_Sig_1 = glm(Default_Payment ~ Credit_Amount, family = "binomial", data=train_data) 

summary(glm_Sig_1)

predict_Sig_1 = predict(glm_Sig_1,test_data,type="response") 

head(predict_Sig_1)

table(train_data$Default_Payment)

predictions_Sig_1 = ifelse(predict_Sig_1 <=0.5, 0,1) 

table(predicted = predictions_Sig_1,actual = test_data$Default_Payment)
confusionMatrix(as.factor(predictions_Sig_1),test_data$Default_Payment, 
                positive="1")


pred_sig_1 <- prediction(predict(glm_Sig_1),train_data$Default_Payment)
perf_sig_1 <- performance(pred_sig_1,"tpr","fpr")
plot(perf_sig_1)

# ----------------------------------- Modedl 2-------------------------------------------------

glm_full_model_2 = glm(Default_Payment ~ Credit_Amount + Gender + Academic_Qualification  +
                         Repayment_Status_Jan + Repayment_Status_March + Repayment_Status_June +
                         Previous_Payment_Jan, family = "binomial", data=train_data) 
summary(glm_full_model_2)

predict_full_model_2 = predict(glm_full_model_2,test_data,type="response") 

head(predict_full_model_2)
table(train_data$Default_Payment)

predictions_full_model_2 = ifelse(predict_full_model_2 <=0.5, 0, 1) 

table(predicted = predictions_full_model_2,actual = test_data$Default_Payment)
confusionMatrix(as.factor(predictions_full_model_2),test_data$Default_Payment, 
                positive="1")

pred_full_model_2 <- prediction(predict(glm_full_model_2),train_data$Default_Payment)
perf_full_model_2 <- performance(pred_full_model_2,"tpr","fpr")
plot(perf_full_model_2)

glm_Sig_2 = glm(Default_Payment ~ Credit_Amount + Gender + Academic_Qualification  +
                  Repayment_Status_Jan + Repayment_Status_March + Repayment_Status_June +
                  Previous_Payment_Jan, family = "binomial", data=train_data) 

summary(glm_Sig_2)

predict_Sig_2 = predict(glm_Sig_2,test_data,type="response") 

head(predict_Sig_2)

table(train_data$Default_Payment)

predictions_Sig_2 = ifelse(predict_Sig_2 <=0.5, 0,1) 

table(predicted = predictions_Sig_2,actual = test_data$Default_Payment)
confusionMatrix(as.factor(predictions_Sig_2),test_data$Default_Payment, 
                positive="1")

pred_sig_2 <- prediction(predict(glm_Sig_2),train_data$Default_Payment)
perf_sig_2 <- performance(pred_sig_2,"tpr","fpr")
plot(perf_sig_2)

############################################ END ###################################################
