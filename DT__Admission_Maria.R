
########################################## Decision Tree ##########################################

# To model a classifier for evaluating the acceptability of Student for admission.

# 1. create a decision tree for the admission data.
# 2. Use rattle to plot the tree.
# 3. Validation of decision tree using the 'Complexity Parameter' and cross validated error.
# 4. Prune the tree on the basis of these parameters to create an optimal decision tree.

########################################## Decision Tree ##########################################

# Step 1: Load Libraries
# -----------------------------------------------------------------------------------

library(caret)
library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

# Step 2: Data Import
# -----------------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Decision Tree\\AdmissionDB")
Admission_df <- read.csv("Admission.csv")
str(Admission_df)
head(Admission_df)

# Step 3: Data Preprocessing
# -----------------------------------------------------------------------------------

Admission_df$Serial.No <- NULL # not IMP so removed
colSums(is.na(Admission_df)) # Checking for null values

# Step 4: Create Model
# -----------------------------------------------------------------------------------

Admission_df <- as.data.frame(Admission_df)
tree <- rpart(Research ~ ., data= Admission_df, method="class")
tree

plot(tree)
text(tree, pretty=0)

fancyRpartPlot(tree)

# Step 5: Validation of Model
# -----------------------------------------------------------------------------------

printcp(tree)
# From the above mentioned list of cp values, we can select the one having the 
# least cross-validated error and use it to prune the tree.
# The value of cp should be least, so that the cross-validated error rate is minimum.

plotcp(tree)
# The cp values are plotted against the geometric mean to depict the deviation until 
# the minimum value is reached. 

# Step 6: Prune the tree to create an optimal decision tree
# -----------------------------------------------------------------------------------

ptree<- prune(tree, cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")

######################################### END ##################################################