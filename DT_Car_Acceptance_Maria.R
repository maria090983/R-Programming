
########################################## Decision Tree ##########################################

# To model a classifier for evaluating the acceptability of car using its given features.
#	 (v1) buying    ->	vhigh, high, med, low
#	 (v2) maint     ->	vhigh, high, med,low
#	 (v3) doors     ->	2, 3, 4, 5 , more
#	 (v4) persons   ->	2, 4, more
#	 (v5) lug_boot  ->	small, med, big.
#	 (v6) safety    ->	low, med, high
#	 (v7) Car Evaluation (Target Variable) ->	 unacc, acc, good, vgood

########################################## Decision Tree ##########################################

# Step 1: Load Libraries
# -----------------------------------------------------------------------------------

library(caret)
#library(rpart)
#library(readr)
#library(caTools)
#library(dplyr)
#library(party)
#library(partykit)
library(rpart.plot)

# Step 2: Data Import
# -----------------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Decision Tree\\CarDB")
car_df <- read.csv("car.data", sep = ',', header = FALSE)
str(car_df)
head(car_df)

# Step 3: Preprocessing & Training
# -----------------------------------------------------------------------------------

## Missing Value Checking
anyNA(car_df)
# OR
colSums(is.na(car_df))

## Summarize details
summary(car_df)

# Step 4: Data Slicing (Spliting Data)
# -----------------------------------------------------------------------------------

set.seed(3033)
intrain <- createDataPartition(y = car_df$V7, p= 0.7, list = FALSE)
training <- car_df[intrain,]
testing <- car_df[-intrain,]

nrow(training)
nrow(testing)
# OR
dim(training)
dim(testing)

# Step 5: Training the Decision Tree classifier with criterion as information gain
# -----------------------------------------------------------------------------------

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# trainControl() method controls the computational of the train() method.
# The "method" parameter holds the details about resampling method.
# The "number" parameter holds the number of resampling iterations.
# The "repeats " parameter contains the complete sets of folds to compute for our repeated cross-validation.

set.seed(3333)
dtree_fit <- train(V7 ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

## Trained Decision Tree classifier results
dtree_fit

# Step 6: Plot Decision Tree
# -----------------------------------------------------------------------------------

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
# This shows the attribute's selection order for criterion as information gain.

# Step 7: Prediction
# -----------------------------------------------------------------------------------

testing[1,]
predict(dtree_fit, newdata = testing[1,])
# For our 1st record of testing data classifier is predicting class variable as "unacc".  
# Now, its time to predict target variable for the whole test set.

test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, testing$V7 )

# Step 8: Training the Decision Tree classifier with criterion as gini index
# -----------------------------------------------------------------------------------

# It is showing us the accuracy metrics for different values of cp. 
# Here, cp is complexity parameter for our dtree.
dtree_fit_gini <- train(V7 ~., data = training, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)
dtree_fit_gini

## Prediction 
test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V7 )  #check accuracy


######################################### END ##################################################