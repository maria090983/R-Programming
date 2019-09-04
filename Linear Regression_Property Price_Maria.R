
###################################### LR MODEL ##############################################

# ------------------------- Property Price prediction ----------------------------------------

##############################################################################################
 
library(readr)
library(naniar)

# Step 1: Load / Import data
#---------------------------------------------------------------------------------------

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Linear Regression\\PropertyDB")

train <- read.csv("Property_Price_Train.csv")
test <- read.csv("Property_Price_Test.csv")
head(train)

# Step 2: Analyse Data
#---------------------------------------------------------------------------------------

# Getting rid of the Id column
test_label <- test$Id

test$ID <- NULL
train$ID <- NULL

# To define same No. of variable/Columns
test$Sale_Price <- NA

# ----------------------------------------
colnames(test)
colnames(train)

# Append/ Mearge rbind - bind by row, cbind - bind by columns
all <- rbind(train, test)

dim(all)

# Check if there are missing values in the data frame using heatmap
vis_miss(all)

str(all)

# Step 3: Missing Value Imputaion
#---------------------------------------------------------------------------------------

# Checking variables that has missing values
# FALSE tells us that the variable has missing values
!colSums(is.na(all))

#Printing the number of missing values for each variable
colSums(is.na(all))

# OR

# Printing only those column names that has missing values along with the missing value counts
col_with_missing_values <- sapply(all, function(x) sum(is.na(x)))
col_with_missing_values[col_with_missing_values>0]

#----------------------------------------------------------------------------------------------
# Replacing NA Values with 'No Access' for Lane_Type variable
# Change the factor class type to character class first
all$Lane_Type<-as.character(all$Lane_Type)

unique(all$Lane_Type)

# Filling NA with No Access
all$Lane_Type[is.na(all$Lane_Type)] <- "No Access"

# Replacing NA values with 'No Basement' for Basement_Height variable
# Check the class of the variable Basement_Height
class(all$Basement_Height)
# Change the factor class type to character class first
all$Basement_Height <- as.character(all$Basement_Height)
# Filling NA with No Access
all$Basement_Height[is.na(all$Basement_Height)] <- "No Basement"

# Same for Basemaent_condition as above
class(all$Basement_Condition)
all$Basement_Condition <- as.character(all$Basement_Condition)
all$Basement_Condition[is.na(all$Basement_Condition)] <- "No Basement"

class(all$Exposure_Level)
all$Exposure_Level <- as.character(all$Exposure_Level)
all$Exposure_Level[is.na(all$Exposure_Level)] <- "No Basement"

class(all$BsmtFinType1)
all$BsmtFinType1 <- as.character(all$BsmtFinType1)
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "No Basement"

class(all$BsmtFinType2)
all$BsmtFinType2 <- as.character(all$BsmtFinType2)
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "No Basement"

class(all$Fireplace_Quality)
all$Fireplace_Quality <- as.character(all$Fireplace_Quality)
all$Fireplace_Quality[is.na(all$Fireplace_Quality)] <- "No Fire Place"

class(all$Garage)
all$Garage <- as.character(all$Garage)
all$Garage[is.na(all$Garage)] <- "No Garge"

class(all$Garage_Built_Year)
#all$Garage_Built_Year <- as.character(all$Garage_Built_Year)
all$Garage_Built_Year[is.na(all$Garage_Built_Year)] <- 0


class(all$Garage_Finish_Year)
all$Garage_Finish_Year <- as.character(all$Garage_Finish_Year)
all$Garage_Finish_Year[is.na(all$Garage_Finish_Year)] <- "No Garge"

class(all$Garage_Condition)
all$Garage_Condition <- as.character(all$Garage_Condition)
all$Garage_Condition[is.na(all$Garage_Condition)] <- "No Garge"

class(all$Garage_Quality)
all$Garage_Quality <- as.character(all$Garage_Quality)
all$Garage_Quality[is.na(all$Garage_Quality)] <- "No Garge"

class(all$Pool_Quality)
all$Pool_Quality <- as.character(all$Pool_Quality)
all$Pool_Quality[is.na(all$Pool_Quality)] <- "No Pool"

class(all$Fence_Quality)
all$Fence_Quality <- as.character(all$Fence_Quality)
all$Fence_Quality[is.na(all$Fence_Quality)] <- "No Fence"

class(all$Miscellaneous_Feature)
all$Miscellaneous_Feature <- as.character(all$Miscellaneous_Feature)
all$Miscellaneous_Feature[is.na(all$Miscellaneous_Feature)] <- "None"
is.na(all$Miscellaneous_Feature)

# Replacing missing values in Lot_Extent with its median value
class(all$Lot_Extent)

all$Lot_Extent[is.na(all$Lot_Extent)] <- median(all$Lot_Extent, na.rm = TRUE)

View(all)

# Using crosstab to generate the count of Brick_Veneer_Type by type of Brick_Veneer_Area
#------------------------------------------------------------------
cross <- table(all$Brick_Veneer_Area,all$Brick_Veneer_Type)
cross_margin <- addmargins(cross)

unique(all$Brick_Veneer_Area)
unique(all$Brick_Veneer_Type)

head(cross_margin,10)

#plot(cross)
#plot(cross_magin)
#plot(all$Brick_Veneer_Area)
#plot(all$Brick_Veneer_Type)
#plot()

#impute the missing values in Brick_Veneer_Type with None and Brick_Veneer_Area with zero
all$Brick_Veneer_Area[is.na(all$Brick_Veneer_Area)] <- 0
unique(all$Brick_Veneer_Area)


all$Brick_Veneer_Type[is.na(all$Brick_Veneer_Type)] <- "None"
unique(all$Brick_Veneer_Type)

# Distribution of the Electrical_System type by Building_Class
cross_elec <- table(all$Electrical_System,all$Building_Class)
cross_elec_margin <- addmargins(cross_elec)

head(cross_elec_margin,10)

class(all$Electrical_System)
all$Electrical_System <- as.character(all$Electrical_System)
all$Electrical_System[is.na(all$Electrical_System)] <- "SBrkr"
unique(all$Electrical_System)

colSums(is.na(all))

col_with_missing_val <- sapply(all, function(x) sum(is.na(x))) 
col_with_missing_val[col_with_missing_val>0]

vis_miss(all)

#Checking the data type for each variable
# ---------------------------------------------------------------------
sapply(all, class)

# Getting list of Numerical Variable and Their Name
numVar <- which (sapply(all, is.numeric))
numVarNames <- names(numVar)
cat("There R", length(numVar), "Numeric Variables")

# Getting list of Caracter Variable and Their Name
numChar <- which (sapply(all, is.character))
numCharNames <- names(numChar)
cat("There R", length(numChar), "Character Variables")

#------------------------------------------------------------------------------------
# For Checking The Correlation Beteween Dependent Vars(Sale Price) and Independent Vars(Other Vars)

library(corrplot)
all_numVar <- all[,numVar]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")

#sort on decreasing correlations with Sale_Price
cor_sorted <- as.matrix(sort(cor_numVar[,'Sale_Price'], decreasing = TRUE))
cor_sorted

#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar)
corrplot.mixed(cor_numVar, lower = "ellipse", upper = "circle")
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

cor_numVar_round <- round(cor_numVar,2)
cor_numVar_round
                
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

# Step 4: Implement model 
#---------------------------------------------------------------------------------------
# Full multiple linear regression model 
model1 <- lm(Sale_Price ~ ., all)

summary(model1)

# predict the Sale_Price with all the X-values

pdct1 = predict(model1, type="response", se.fit=FALSE)

length(pdct1)

# table showing the difference between the actual and predicted values
actual_Sale_Price = train$Sale_Price
length(actual_Sale_Price)

pred_Sale_Price = pdct1
difference = round((actual_Sale_Price- pred_Sale_Price),2)
df_Sale_Price = data.frame(actual_Sale_Price, pred_Sale_Price,difference)
View(df_Sale_Price)

## ------------------------------------------------------------------------
# Collect statistics associated with each variabel
# Loading the broom library to use the tidy function

library(broom)
tidy_model = tidy(model1)
options(scipen = 999)
# This gives statistics associated with all the variables
tidy_model

## ------------------------------------------------------------------------
#Collecting all the variables that has p-value less than 0.05

sigvar <- tidy_model$term[tidy_model$p.value < 0.05]
sigvar

sigdf <- c(sigvar,"Sale_Price")
data1 <- all[,names(all) %in% sigdf]
sigvar_model <- lm(Sale_Price ~., data = data1)

summary(sigvar_model)

## ------------------------------------------------------------------------
# predict the Sale_Price with all the significant variables

pdct_sig <- predict(sigvar_model, type = "response", se.fit = FALSE)

# table showing the difference between the actual and predicted values
actual_Sale_Price_sig = train$Sale_Price
length(actual_Sale_Price_sig)

pred_Sale_Price_sig = pdct_sig

difference = round((actual_Sale_Price_sig - pred_Sale_Price_sig),2)

df_Sale_Price = data.frame(actual_Sale_Price_sig, pred_Sale_Price_sig,difference)

View(df_Sale_Price)


## ------------------------------------------------------------------------
fit <- lm(Sale_Price ~ Lot_Size + Overall_Material * House_Condition * Construction_Year + Brick_Veneer_Area+ First_Floor_Area*Second_Floor_Area *  Kitchen_Above_Grade, data = data1)


## ------------------------------------------------------------------------
all$Construction_Year<-NULL


## ------------------------------------------------------------------------
summary(fit)

plot(fit)
plot(all)

########################################## END ###############################################