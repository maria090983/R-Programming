
# ------------------------------ Price Pridiction of New Appartment ------------------------
# Company wants to decide price of new appartment based on existing(Builtin) data

# Step 1: Data Aequisition
# step 2: Divide Datset(Train & Test)
# Step 3: EDA
# Step 4: Implement Model
# Step 5: Optimize Model
# Step 6: Model Validation
# Step 7: Prediction
# ------------------------------------------------------------------------------------------

# ------------------------ Step 1: ----------------------------------------

library(MASS)
data("Boston")
?Boston
View(Boston)
colSums(is.na(Boston))

# ------------------------ Step 2: ---------------------------------------

set.seed(2)
library(caTools)    # Split function present inti this library
split_data <- sample.split(Boston, SplitRatio = 0.7)
split_data

training_data <- subset(Boston, split_data=="TRUE")
testing_data <- subset(Boston, split_data=="FALSE")

# ----------------------- Step 3: -----------------------------------------

plot(Boston$crim, Boston$medv, cex=0.5, xlab = "Crime Rate", ylab = "Price", col="magenta")
scatter.smooth(Boston$crim,Boston$medv,xlab = "Crime Rate", ylab = "Price", col="red")

# Find Co-Relation among all variables through scatter plot
library(corrplot)
Co_rel <- cor(Boston)
Co_rel
corrplot(Co_rel, type = "lower")
corrplot(Co_rel, method = "number")

attach(Boston)

library(lattice)
splom(~Boston[c(1:7)], groups = NULL, data = Boston)
splom(~Boston[c(8:14)], groups = NULL, data = Boston)

# Plot Fitted line
plot(rm,medv)
abline(lm(medv~rm), col="red")

# Find Multicollinearity
library(caret)
# To exclude medv(o/p)
Boston_a <- subset(Boston, select = -c(medv))
numeric_data <- Boston_a[sapply(Boston_a, is.numeric)]
descr_cor <- cor(numeric_data)
corrplot(descr_cor, type = "lower")
corrplot(descr_cor, method = "number")

# Find Variance Inflation Factor - measures increase in variance of an estimated regression coefficient due to multicollinearity
library(car)
model <- lm(medv ~., data = training_data)
model
vif_model <- vif(model)
vif_model
# vif = 1 : no corelation among variables
# vif = high : high multicollinearity (e.g. rad & tax)
library(corrgram)    # correlogram
corrgram(training_data, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)
corrgram(training_data, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pts, diag.panel = panel.minmax)
corrgram(training_data, order = TRUE, lower.panel = panel.shade, upper.panel = NULL, text.panel = panel.txt)
corrgram(training_data)

summary(model)

# ------------------------------- Step 4 --------------------------------------

model1 <- lm(medv~., data = training_data)
model1
summary(model1)

# ------------------------------- Step 5 ----------------------------------------

model2 <- lm(medv~ crim + nox + rm + dis + rad + ptratio + black + lstat, data = training_data)
model2
summary(model2)

model3 <- lm(medv~ crim + zn + chas + rm + rad + dis + rad, data = training_data)
model3
summary(model3)

model4 <- lm(medv~ crim + zn + indus + chas + nox + rm + age + dis + rad + ptratio + black + lstat, data = training_data)
model4
summary(model4)

# Check R square val
# If R square val is closer to -> 1.0, then the linear model is best suited

# ----------------------------- Step 6 ----------------------------------------

predic_data1 <- predict(model1, testing_data)
predic_data1

predic_data2 <- predict(model2, testing_data)
predic_data2

predic_data3 <- predict(model3, testing_data)
predic_data3

predic_data4 <- predict(model4, testing_data)
predic_data4

plot(testing_data$medv, type = "l", lty = 1.8, col = "green")

l1 <- lines(predic_data1, type = "l", col = "Dark blue")
l2 <- lines(predic_data2, type = "l", col = "red")
l3 <- lines(predic_data3, type = "l", col = "magenta")
l4 <- lines(predic_data4, type = "l", col = "black")

# --------------------------- Step 7 --------------------------------------
actual_Vs_preds1 <- data.frame(cbind(Actual_Price=testing_data$medv, Predicted_Price=predic_data1))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actual_Vs_preds)  # 82.7%
head(actual_Vs_preds1)
View(actual_Vs_preds1)
boxplot(actual_Vs_preds1)

#actual_Vs_preds2 <- data.frame(cbind(Actual_Price=testing_data$medv, Predicted_Price=predic_data2))
#View(actual_Vs_preds2)

#actual_Vs_preds3 <- data.frame(cbind(Actual_Price=testing_data$medv, Predicted_Price=predic_data3))
#View(actual_Vs_preds3)

#actual_Vs_preds4 <- data.frame(cbind(Actual_Price=testing_data$medv, Predicted_Price=predic_data4))
#View(actual_Vs_preds4)


All_Pred_prices <- data.frame(Actual_Price=testing_data$medv,Pr_Price1=predic_data1,
                   Pr_Price2=predic_data2, Pr_Price3=predic_data3, Pr_Price4=predic_data4)
View(All_Pred_prices)

boxplot(All_Pred_prices)

###################################### END ###############################################