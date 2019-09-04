

################################# Logistic Regression Model ###############################

# Predict passengers r survived or not in titanic

################################# Logistic Regression Model ###############################
# Try to Detect outliers and remove it
# Calculate VIF
# Used Step() function for model optimization/ Revise model
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
hist(titanic_data$Age)
bx <- boxplot(titanic_data$Age)
bx$stats

# ------- Renaming Variable --------------

levels(titanic_data$Sex)[levels(titanic_data$Sex) == "male"] <- "1"
levels(titanic_data$Sex)[levels(titanic_data$Sex) == "female"] <- "0"

levels(titanic_data$Embarked)[levels(titanic_data$Embarked) == "C"] <- "1"
levels(titanic_data$Embarked)[levels(titanic_data$Embarked) == "Q"] <- "2"
levels(titanic_data$Embarked)[levels(titanic_data$Embarked) == "S"] <- "3"

titanic_data = titanic_data[-c(3, 4, 9)]
head(titanic_data)
boxplot(titanic_data$Age)

quantile(titanic_data$Age, seq(0, 1, 0.02))
titanic_data$Age = ifelse(titanic_data$Age >=52, 52, titanic_data$Age)
titanic_data$Age = ifelse(titanic_data$Age<=4, 4, titanic_data$Age)
boxplot(titanic_data$Age)

bx = boxplot(titanic_data$Fare)
bx$stats
quantile(titanic_data$Fare, seq(0, 1, 0.02))
titanic_data$Fare = ifelse(titanic_data$Fare >= 136, 136, titanic_data$Fare)
boxplot(titanic_data$Fare)

# ----------- Let us now start our bivariate analysis --------------------------

library(car)
scatterplot(titanic_data$Age, titanic_data$Survived)
scatterplot(titanic_data$Fare, titanic_data$Survived)

# ---------- Let's now divide the data into train and test sets ---------------

set.seed(222)
t= sample(1:nrow(titanic_data), 0.7*nrow(titanic_data))
train = titanic_data[t,]
test = titanic_data[-t,]
nrow(train)
nrow(test)

# --------------------------------- Build Model -------------------------------

library(car)
model <- lm(Survived~., data=train)
model
t = vif(model)  # variance inflation factor : tells factors r correlated to each other
t
sort(t, decreasing=TRUE)

# ------------------------------Revise / Optimize Model ------------------------

model1<- glm(as.factor(Survived)~., family="binomial", data=train)
summary(model1)

stepmodel = step(model1, direction="both")   # step uses add1 model and drop1 model repeatedly
formula(stepmodel)

## as.factor(survived) ~ pclass + age + sibsp + female + embarked_c
summary(stepmodel)

train$score <- predict(stepmodel, newdata = train, type="response")
head(train$score)
tail(train$score)

library(lattice)
library(ggplot2)
library(caret)
library(e1071)

train$prediction <- ifelse(train$score>=0.6, 1, 0)
table(Predicted_Value = factor(train$prediction), Actual_Value = factor(train$Survived))

# accuracy = (TP+TN)/(TP+TN+FP+FN)=(518+263)/(518+83+52+263)=0.852=85.2%

# ------------------------------------------------------------

#library(InformationValue)


#plotROC(Actuals=train$Survived, predictedScores=as.numeric(fitted(stepmodel)))
#ks_plot(actuals=train$Survived, predictedScores=as.numeric(fitted(stepmodel)))

test$score<-predict(stepmodel, test, type = "response")
head(test$score)

test$predicted<-ifelse(test$score>=0.6, 1, 0)
head(test$predicted)

table(factor(test$predicted), factor(test$Survived))

library(ROCR)
ROCRPred <- prediction(test$score, test$Survived)
ROCRPred
ROCRPerf <- performance(ROCRPred, measure = "tpr", x.measure = "fpr")
ROCRPerf
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 01))

############################################## END ###########################################




