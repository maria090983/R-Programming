
########################################## Decision Tree ##########################################

#Problem Statement: To study a Mushroom data set in order to predict whether a given mushroom is 
# edible or poisonous to human beings.

########################################## Decision Tree ##########################################

# Step 1: load libraries
# ---------------------------------------------------------------------------------------------

library(rpart)
library(caret)
library(rpart.plot)
library(rattle)

# Step 2: Import the data set
# ---------------------------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Decision Tree\\MushroomsDB")
mushrooms_data <- read.csv ("mushrooms.csv")
str(mushrooms_data)
head(mushrooms_data)

# Step 3: Data Cleaning 
# ---------------------------------------------------------------------------------------------

## Number of rows with missing values
nrow(mushrooms_data) - sum(complete.cases(mushrooms_data))
#
# OR
colSums(is.na(mushrooms_data))

## Deleting redundant information variable "veil.type"
mushrooms_data$veil.type <- NULL

# Step 4: Data Exploration and Analysis
# ---------------------------------------------------------------------------------------------
# To get a good understanding of the 21 predictor variables, I've created a table for each 
# predictor variable vs class type (response/ outcome variable) in order to understand
# whether that particular predictor variable is significant for detecting the output or not.
# In Class Veriable - 'e' stands for edible class and 'p' stands for the poisonous class

## Analyzing the variable

table(mushrooms_data$class,mushrooms_data$odor)
table(mushrooms_data$class,mushrooms_data$cap.shape)
table(mushrooms_data$class,mushrooms_data$cap.surface)
table(mushrooms_data$class,mushrooms_data$cap.color)
table(mushrooms_data$class,mushrooms_data$bruises)
table(mushrooms_data$class,mushrooms_data$gill.attachment)
table(mushrooms_data$class,mushrooms_data$gill.spacing)
table(mushrooms_data$class,mushrooms_data$gill.size)
table(mushrooms_data$class,mushrooms_data$gill.color)
table(mushrooms_data$class,mushrooms_data$stalk.shape)
table(mushrooms_data$class,mushrooms_data$stalk.root)
table(mushrooms_data$class,mushrooms_data$stalk.surface.above.ring)
table(mushrooms_data$class,mushrooms_data$stalk.surface.below.ring)
table(mushrooms_data$class,mushrooms_data$stalk.color.above.ring)
table(mushrooms_data$class,mushrooms_data$stalk.color.below.ring)
table(mushrooms_data$class,mushrooms_data$ring.number)
table(mushrooms_data$class,mushrooms_data$ring.type)
table(mushrooms_data$class,mushrooms_data$spore.print.color)
table(mushrooms_data$class,mushrooms_data$population)
table(mushrooms_data$class,mushrooms_data$habitat)

# The above output shows that the mushrooms with odor values 'c', 'f', 'm', 'p', 's' 
# and 'y' are clearly poisonous. And the mushrooms having almond (a) odor (400) are edible. 
# Such observations will help us to predict the output class more accurately.

## predict which variable would be the best one for splitting the Decision Tree.
number.perfect.splits <- apply(X = mushrooms_data[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms_data$class,col)
  sum(t == 0)
})

## Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

## Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")
# The output shows that the 'odor' variable plays a significant role in predicting the output class of the mushroom.

# Step 5: Data Splicing (Divide data in Test & Train)
# ---------------------------------------------------------------------------------------------

##data splicing
set.seed(12345)
train <- sample(1:nrow(mushrooms_data),size = ceiling(0.80*nrow(mushrooms_data)),replace = FALSE)
## training set
mushrooms_train <- mushrooms_data[train,]
## test set
mushrooms_test <- mushrooms_data[-train,]

# To minimize the number of poisonous mushrooms misclassified as edible we will assign 
# a penalty 10x bigger, than the penalty for classifying an edible mushroom as poisonous
## penalty matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
penalty.matrix

#Step 6: Building a model
# ---------------------------------------------------------------------------------------------
# rpart  (Recursive Partitioning And Regression Trees) algorithm:

## building the classification tree with rpart
tree <- rpart(class~.,
              data=mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")

# Step 7: Visualising the tree
# ---------------------------------------------------------------------------------------------

rpart.plot(tree, nn=TRUE)

# Step 8: Testing the model
# ---------------------------------------------------------------------------------------------

pred <- predict(object=tree,mushrooms_test[-1],type="class")

# Step 9: Calculating accuracy
# ---------------------------------------------------------------------------------------------

t <- table(mushrooms_test$class,pred) 
confusionMatrix(t)

# The output shows that all the samples in the test dataset have been correctly classified 
# and an accuracy of 100% on the test data set with a 95% confidence interval (0.9977, 1).
# Thus we can correctly classify a mushroom as either poisonous or edible using this Decision Tree model.

########################################### END ###############################################