################################# Logistic Regression Model ###############################

# Predict passengers r survived or not in titanic

################################# Logistic Regression Model ###############################

#set working directory
path <- "G:\\MARIA\\DSP Training\\R Programing\\Practice\\Logistic Regression\\Titanic_Dataset"
setwd(path)

#Load libraries and data
library (data.table)
library (plyr)
library (stringr)
train <- fread("train.csv",na.strings = c(""," ",NA,"NA"))
test <- fread("test.csv",na.strings = c(""," ",NA,"NA"))
head(train)
head(test)
str(train)
# This data set comprises a mix of character and numeric variables.
# Among these variables, Survived is the dependent variable. 
# To get a deeper understanding of these variables, let's do a quick data exploration. 
# Discoveries from this step will help us determine what to do next:
#check missing values
colSums(is.na(train))
colSums(is.na(test))
# We see that variable Age and Cabin have missing values.
# Interestingly, the variable Fare has one missing value only in the test set. 
# start exploring each variable individually. For numeric variables, use the summary function. 
# For character / factor variables, use table for exploration:
#Quick Data Exploration
summary(train$Age)
summary(test$Age)

train[,.N/nrow(train),Pclass]
test[,.N/nrow(test),Pclass]

train [,.N/nrow(train),Sex]
test [,.N/nrow(test),Sex]

train [,.N/nrow(train),SibSp]
test [,.N/nrow(test),SibSp]

train [,.N/nrow(train),Parch]
test [,.N/nrow(test),Parch] #extra 9

summary(train$Fare)
summary(test$Fare)

train [,.N/nrow(train),Cabin]
test [,.N/nrow(test),Cabin]

train [,.N/nrow(train),Embarked] 
test [,.N/nrow(test),Embarked]
# Following are the insights we can derive from the data exploration above:
#   The variable Fare is skewed (right) in nature. We'll have to log transform it such that it resembles a normal distribution.
# The variable Parch has one extra level (9) in the test set as compared to the train set. We'll have to combine it with its mode value.
# A smart way to make modifications in train and test data is by combining them. This way, you'll save yourself from writing some extra lines of code. I suggest you follow every line of code carefully and simultaneously check how every line affects the data.

#combine data
alldata <- rbind(train,test,fill=TRUE)

#New Variables
#Extract passengers title
alldata [,title := strsplit(Name,split = "[,.]")]
alldata [,title := ldply(.data = title,.fun = function(x) x[2])]
alldata [,title := str_trim(title,side = "left")]

#combine titles
alldata [,title := replace(title, which(title %in% c("Capt","Col","Don","Jonkheer","Major","Rev","Sir")), "Mr"),by=title]
alldata [,title := replace(title, which(title %in% c("Lady","Mlle","Mme","Ms","the Countess","Dr","Dona")),"Mrs"),by=title]

#ticket binary coding
alldata [,abs_col := strsplit(x = Ticket,split = " ")]
alldata [,abs_col := ldply(.data = abs_col,.fun = function(x)length(x))]
alldata [,abs_col := ifelse(abs_col > 1,1,0)]
# Next, we'll impute missing values, transform Fare variable and remove an extra level from Parch variable. This will make our data ready for machine learning.
#Impute Age with Median
for(i in "Age")
set(alldata,i = which(is.na(alldata[[i]])),j=i,value = median(alldata$Age,na.rm = T))

#Remove rows containing NA from Embarked
alldata <- alldata[!is.na(Embarked)]

#Impute Fare with Median
for(i in "Fare")
set(alldata,i = which(is.na(alldata[[i]])),j=i,value = median(alldata$Fare,na.rm = T))

#Replace missing values in Cabin with "Miss"
alldata [is.na(Cabin),Cabin := "Miss"]

#Log Transform Fare
alldata$Fare <- log(alldata$Fare + 1)

#Impute Parch 9 to 0
 alldata [Parch == 9L, Parch := 0]
# The method of using for - set loop for imputing missing values works blazing fast on large data sets. In our case, the data set is small, hence it's difficult to note the difference. Now, our data set is ready. Let's implement Logistic Regression and check our model's accuracy.
#Collect train and test
train <- alldata[!(is.na(Survived))]
train [,Survived := as.factor(Survived)]

test <- alldata[is.na(Survived)]
test [,Survived := NULL]

#Logistic Regression
model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = train[,-c("PassengerId","Name","Ticket")])
summary(model)
# In R, you can implement Logistic Regression using the glm function. Now, let's understand and interpret the crucial aspects of summary:
#   The glm function internally encodes categorical variables into n - 1 distinct levels.
# Estimate represents the regression coefficients value. Here, the regression coefficients explain the change in log(odds) of the response variable for one unit change in the predictor variable.
# Std. Error represents the standard error associated with the regression coefficients.
# z value is analogous to t-statistics in multiple regression output. z value > 2 implies the corresponding variable is significant.
# p value determines the probability of significance of predictor variables. With 95% confidence level, a variable having p < 0.05 is considered an important predictor. The same can be inferred by observing stars against p value.
# In addition, we can also perform an ANOVA Chi-square test to check the overall effect of variables on the dependent variable.

#run anova
anova(model, test = 'Chisq')
# logistic output 1
# Following are the insights we can collect for the output above:
  
#   In the presence of other variables, variables such as Parch, Cabin, Embarked, and abs_col are not significant. We'll try building another model without including them.
# The AIC value of this model is 883.79.
# Let's create another model and try to achieve a lower AIC value.

model2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + title, data = train,family = binomial(link="logit"))
summary(model2)
#glm table
#As you can see, we've achieved a lower AIC value and a better model. Also, we can compare both the models using the ANOVA test. Let's say our null hypothesis is that second model is better than the first model. p < 0.05 would reject our hypothesis and in case p > 0.05, we'll fail to reject the null hypothesis.

#compare two models
anova(model,model2,test = "Chisq")
#anova output
#With p > 0.05, this ANOVA test also corroborates the fact that the second model is better than first model. Let's predict on unseen data now. Since, we can't evaluate a model's performance on test data locally, we'll divide the train set and use model 2 for prediction.

#partition and create training, testing data
library(caret)
split <- createDataPartition(y = train$Survived,p = 0.6,list = FALSE)

new_train <- train[split] 
new_test <- train[-split]

#model training and prediction
log_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + title, data = new_train[,-c("PassengerId","Name","Ticket")],family = binomial(link="logit"))
log_predict <- predict(log_model,newdata = new_test,type = "response")
log_predict <- ifelse(log_predict > 0.5,1,0)
#For now, I've set the probability threshold value as 0.5. Let's get the flavor of our model's accuracy. We'll use AUC-ROC score to determine model fit. Higher the score, better the model. You can also use confusion matrix to determine accuracy using confusionMatrix function from caret package.
#plot ROC 
library(ROCR) 
library(Metrics)
pr <- prediction(log_predict,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf) > auc(new_test$Survived,log_predict) #0.76343
#AUC roc plot logistic regression
#Our AUC score is 0.763. As said above, in ROC plot, we always try to move up and top left corner. From this plot, we can interpret that the model is predicting more negative values incorrectly. To move up, let's increase our threshold value to 0.6 and check the model's performance.

log_predict <- predict(log_model,newdata = new_test,type = "response")
log_predict <- ifelse(log_predict > 0.6,1,0)

pr <- prediction(log_predict,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
auc(new_test$Survived,log_predict) #0.8008
#auc plot probability threshold logistic regression
#Now, our AUC has increased to 0.80 along with a slight uplift in the ROC curve. With this, we've reached to the end of this tutorial. You can try and test AUC value for other values of probability threshold as well. From here, I would request you go ahead and test your model on the original test set, upload your solution and check your kaggle rank. Moving beyond Logistic Regression, you can further improve your model's accuracy using tree-based algorithms such as Random Forest or XGBoost. The complete code for this tutorial is also available on Github.
