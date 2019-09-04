

################################# Logistic Regression Model ###############################

# Predict passengers r survived or not in titanic

################################# Logistic Regression Model ###############################
# Create Dummy variables for catogorical variable and try to implement model
###########################################################################################

# Step 1: Load data
#--------------------------------------------------------------------------------------------
setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Logistic Regression\\Titanic_Dataset")
titanic_data <- read.csv("Titanic_Dataset.csv")
str(titanic_data)
View(titanic_data)
head(titanic_data)

summary(titanic_data)

# Step 2: Missing value Imputation
#--------------------------------------------------------------------------------------------

colSums(is.na(titanic_data)) # Check Missing Values
colSums(titanic_data=="") # Check Blank Spaces

library(DMwR)
library(Amelia)

missmap(titanic_data)  # included in Amelia package
titanic_data <- centralImputation(titanic_data)
colSums(is.na(titanic_data))
missmap(titanic_data)
View(titanic_data)

titanic_data$Embarked[titanic_data$Embarked == ""] <- "S"

# Removing Cabin as it has high missing values(blank values)
library(dplyr)
titanic_data <- titanic_data %>% select(-c(Cabin)) 
View(titanic_data)
missmap(titanic_data)
colSums(is.na(titanic_data)) # Check Missing Values
colSums(titanic_data=="") # Check Blank Spaces

# Step 3: Create Data frame for Dependent & independent Variables (remove non significant vars)
#--------------------------------------------------------------------------------------------

df_non_vars <- c("Name", "Ticket")
titanic_data <- titanic_data[, !(names(titanic_data) %in% df_non_vars)]
str(titanic_data)
# OR
# titanic_data <- titanic_data %>% select(-c(Cabin, PassengerId, Name, Ticket, Embarked))

# Step 4: Convert Categorical variables to factor
#--------------------------------------------------------------------------------------------

for (i in c("Survived","Pclass", "Sex", "Embarked")) 
{
  titanic_data[,i] = as.factor(titanic_data[,i])
  
}

# Step 5: Create dummy variables for categorical vars
#--------------------------------------------------------------------------------------------
str(titanic_data)

library(dummies)
titanic_data <- dummy.data.frame(titanic_data, names = c("Pclass", "Sex", "Embarked"), sep = "_")

# Step 6: Divide data (Test & Train)
#--------------------------------------------------------------------------------------------

library(caTools)

split_data <- sample.split(titanic_data, SplitRatio = 0.8)
split_data

train_data <- subset(titanic_data, split_data == "TRUE")
test_data <- subset(titanic_data, split_data == "FALSE")
nrow(train_data)
nrow(test_data)

# OR (Other Way)
#train_data <- titanic_data[1:1047,]
#test_data <- titanic_data[1048:1309,]

# Step 5: Check for Multicolinearity
#--------------------------------------------------------------------------------------------

train_data$Survived <- as.numeric(train_data$Survived)
str(train_data)
cor(train_data)
train_data$Survived <- as.factor(train_data$Survived)

# Step 6: Build Logit Model on Train Data
#--------------------------------------------------------------------------------------------

titanic_model1 <- glm(Survived ~., data = train_data, family = "binomial" )
summary(titanic_model1)

# Step 7: Optimize Model / Revise Model
#--------------------------------------------------------------------------------------------

titanic_model2 <- glm(Survived ~. -Parch, data = train_data, family = "binomial" )
summary(titanic_model2)

titanic_model3 <- glm(Survived ~. -Parch -Fare, data = train_data, family = "binomial" )
summary(titanic_model3)

# Anayse table of Deviance
anova(titanic_model2, test = "Chisq")

# Step 8: Test Accuracy (Build Confusion matrix)
#--------------------------------------------------------------------------------------------

predict_train <- predict(titanic_model2, data = train_data, type = "response")
predict_train

# Confusion matrix
table(Actual = train_data$Survived, Predicted = predict_train >= 0.5)

# Step 9: Predict survivability for test data
# -------------------------------------------------------------------------------------------

titanic_model2_test <- glm(Survived ~. -Parch, data = test_data, family = "binomial" )
summary(titanic_model2_test)

predict_test <- predict(titanic_model2_test, data = test_data, type = "response")
predict_test <- ifelse(predict_test >= 0.5,1,0)
predict_test

# Confusion Matrix
#library(caret)
#confusionMatrix(data = predict_test, reference = test_data$Survived)
# OR
table(Actual = test_data$Survived, Predicted = predict_test)

# Draw ROCR Curve

library(ROCR)
ROCRPred <- prediction(predict_test, test_data$Survived)
ROCRPred
ROCRPerf <- performance(ROCRPred, measure = "tpr", x.measure = "fpr")
ROCRPerf
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 01))

# Write File
Predictions <- data.frame(test_data[c("PassengerId","Survived")])
Predictions
write.csv(file = "Titanic_Predicted_Maria.csv", x = Predictions)

############################################## END ################################################

