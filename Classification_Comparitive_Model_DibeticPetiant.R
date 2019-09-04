
################################# Classification Models ###################################
 
# Using Predictive Models to Classify Pima Indians Diabetes Database

################################# Classification Models ###################################

# Load Libraries
# ------------------------------------------------------------------------------------------
  
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)

# Load Data
# ------------------------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Classification_Comparitive Model\\DibetecDB")
diabetes <- read.csv('diabetes.csv')
dim(diabetes)
str(diabetes)
head(diabetes)


# Exploratory Data Analysis and Feature Selection
# ------------------------------------------------------------------------------------------

##Check missing values 
cat("Number of missing value:", sum(is.na(diabetes)), "\n")

## Staitstical summary 
summary(diabetes)

diabetes$Type <- factor(diabetes$Type)

## Histogram of numeric variables
p1 <- ggplot(diabetes, aes(x=Npreg)) + ggtitle("Number of times pregnant") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
p2 <- ggplot(diabetes, aes(x=Glu)) + ggtitle("Glucose") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 5, colour="black", fill="white") + ylab("Percentage")
p3 <- ggplot(diabetes, aes(x=BP)) + ggtitle("Blood Pressure") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="white") + ylab("Percentage")
p4 <- ggplot(diabetes, aes(x=Skin)) + ggtitle("Skin Thickness") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="white") + ylab("Percentage")
p5 <- ggplot(diabetes, aes(x=Ins)) + ggtitle("Insulin") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 20, colour="black", fill="white") + ylab("Percentage")
p6 <- ggplot(diabetes, aes(x=BMI)) + ggtitle("Body Mass Index") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
p7 <- ggplot(diabetes, aes(x=PedF)) + ggtitle("Diabetes Pedigree Function") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage")
p8 <- ggplot(diabetes, aes(x=Age)) + ggtitle("Age") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=1, colour="black", fill="white") + ylab("Percentage")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)
# All the variables have broad distribution, therefore, will be kept for the regression analysis. 

## Correlation Between Numeric Varibales
numeric.var <- sapply(diabetes, is.numeric)
corr.matrix <- cor(diabetes[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", order = "hclust", tl.col = "black", tl.srt=45, tl.cex=0.5, cl.cex=0.5)
# The numeric variabls are almost not correlated. 

## Correlation bewteen numeric variables and outcome. 
attach(diabetes)
par(mfrow=c(2,4))
boxplot(Npreg~Type, main="No. of Pregnancies vs. Diabetes", 
        xlab="Outcome", ylab="Pregnancies")
boxplot(Glu~Type, main="Glucose vs. Diabetes", 
        xlab="Outcome", ylab="Glucose")
boxplot(BP~Type, main="Blood Pressure vs. Diabetes", 
        xlab="Outcome", ylab="Blood Pressure")
boxplot(Skin~Type, main="Skin Thickness vs. Diabetes", 
        xlab="Outcome", ylab="Skin Thickness")
boxplot(Ins~Type, main="Insulin vs. Diabetes", 
        xlab="Outcome", ylab="Insulin")
boxplot(BMI~Type, main="BMI vs. Diabetes", 
        xlab="Outcome", ylab="BMI")
boxplot(Npreg~Type, main="Diabetes Pedigree Function vs. Diabetes", xlab="Outcome", ylab="DiabetesPedigreeFunction")
boxplot(diabetes$Age~diabetes$Type, main="Age vs. Diabetes", xlab="Outcome", ylab="Age")
# Blood pressure and skin thickness show little variation with diabetes, 
# they will be excluded from the model. Other variables show more or less 
# correlation with diabetes, so will be kept.

diabetes$BP <- NULL
diabetes$Skin <- NULL

## Split Dataset
train <- diabetes[1:540,]
test <- diabetes[541:768,]

############################ Implement Logistic Regression Model #############################

## Implement model
model <-glm(Type ~.,family=binomial(link='logit'),data=train)
summary(model)
#The top 3 most relevant features are "Glucose", "BMI" and "Number of times pregnant" because of the low p-values.
#"Insulin" and "Age" appear not statistically significant.

anova(model, test="Chisq")
# From the table of deviance, we can see that adding insulin and age have little effect on the residual deviance.

## Cross Validation & Accuracy
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Type)
glm_acc <- print(paste('Accuracy',1-misClasificError)) ; glm_acc

############################### Implement Decision Tree #################################

library(rpart)

## Implement model
model2 <- rpart(Type ~ Npreg + Glu + BMI + PedF, data=train,method="class")

## Visual Representation of model
plot(model2, uniform=TRUE, 
     main="Classification Tree for Diabetes")
text(model2, use.n=TRUE, all=TRUE, cex=.8)
fancyRpartPlot(model2)
# This means if a person's BMI less than 45.4 and her diabetes digree function less than 0.8745, then she is more likely to have diabetes. 

## Confusion table and accuracy
treePred <- predict(model2, test, type = 'class')
table(treePred, test$Type)
mean(treePred==test$Type) # It Gives accuracy = 74.56% 
# Need to improve Accuracy so do pruning

# Prun Model (Remove unwanted branches)
# ------------------------------------------------------------------------------------------

prun <- prune(model2, cp = 0.02); pruned
fancyRpartPlot(prun)

Pred_prun <- predict(prun, test, type = "class")
table(test$Type, Pred_prun)
dt_acc <- mean(test$Type==Pred_prun); dt_acc # It Gives accuracy = 77.63% means accuracy improved

# Compare Model graphically
# ------------------------------------------------------------------------------------------
accuracy <- data.frame(Model=c("Logistic Regression","Decision Tree"), 
                       Accuracy=c(glm_acc, dt_acc))
ggplot(accuracy,aes(x=Model,y=Accuracy)) + 
  geom_bar(stat='identity') + theme_linedraw() + 
  ggtitle('Comparison of Model Accuracy')

# Result: Logistic Regression gave better Accuracy than decision tree

####################################### END ##############################################