# ------------------------------------ MtCars Project ------------------------------------------

# Lets build a simple regression model that we can use to predict Distance
# (dist) by establishing a statistically significant linear relationship with Speed (speed).

# Step 1 : Load DataSet(Builtin)
# --------------------------------------------------------------------------------

View(cars)
head(cars)  

# Step 2: Scatter Plot -> help visualize any linear relationships between the dependent (response) variable and independent (predictor) variables.
#---------------------------------------------------------------------------------

scatter.smooth(x=cars$speed,xlab = "CarSpeed", y=cars$dist, ylab = "CarDiatance", main="Dist Vs Speed")  # scatterplot

# Step 3: Box Plot
# ---------------------- BoxPlot - Check for outliers ----------------------------
#---------------------------------------------------------------------------------

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'
# OR
boxplot(cars$speed,cars$dist, main="Speed & Distance")

# Step 4: Density Plot
#----- Density plot - Check if the response variable is close to normality---------
#-----------------------------------------------------------------------------------

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="green")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

# Step 5: Correlation (Between +1 to -1)
# -------------------- calculate correlation between speed and distance ------------

m1 <- cor(cars$speed, cars$dist)  
m1

# Step 6: Build LR Model
# ------------------------------ Build Linear Model --------------------------------

linear_Model <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linear_Model)

# Step 7: Check Summary of LR model
# -------------------------- Linear Regression Diagnostics -------------------------

summary(linear_Model) # Check the significance levels

# OR

#The p Value: Checking for statistical significance

modelSummary <- summary(linear_Model)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients

beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linear_Model$fstatistic[1]  # fstatistic
f <- summary(linear_Model)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

# Step 8: Create Train & Test Datasets
# Create the training (development) and test (validation) data samples from original data.
# -------------------------------------------------------------------------------

set.seed(100)  # setting seed to reproduce results of random sampling
Sampling_data <- sample(0.8*nrow(cars))  # row indices for training data
training_Data <- cars[Sampling_data, ]  # model training data
test_Data  <- cars[-Sampling_data, ]   # test data

# Step 9: Develop Model using Train Data
#Develop the model on the training data and use it to predict the distance on test data
# -------------------- Build the model on training data --------------------------------

lr_Model <- lm(dist ~ speed, data=training_Data)  # build the model
dist_Pred <- predict(lr_Model, test_Data)  # predict distance

# Step 10: Revive Statistics of Train Model
# --------------------------- Review diagnostic measures ---------------------------

summary (lr_Model)  # model summary

# Step 11: 
# --------------- Calculate prediction accuracy and error rates -------------------------

actual_Vs_preds <- data.frame(cbind(Actual_Dist=test_Data$dist, Predicted_Dist=dist_Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actual_Vs_preds)  # 82.7%
head(actual_Vs_preds)
View(actual_Vs_preds)

# ---------- Visualize Representation of Model -----------------------
act_plot <- plot(test_Data$dist,main = "Actual Distance")
pre_plot <- plot(dist_Pred, main = "Predicted Distance")

scatter.smooth(test_Data$dist, main = "Actual Distance", col="red")
scatter.smooth(dist_Pred, main = "Predicted Distance", col="green")


#--------------------------------------END----------------------------------------------------