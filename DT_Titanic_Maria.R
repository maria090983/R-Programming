
########################################## Decision Tree ##########################################

# created a Decision tree that classifies a Passengers in Titanic were Survived or not.

########################################## Decision Tree ##########################################

# Load Libraries

library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)
library(rattle)


# Load Data
# ----------------------------------------------------------------

titanic_data <- "https://goo.gl/At238b" %>%     #DataFlair
read.csv %>% # read in the data
select(survived, embarked, sex, 
sibsp, parch, fare) %>%
mutate(embarked = factor(embarked), Sex = factor(sex))

# Divide Data
# ----------------------------------------------------------------

set.seed(123)
sample_data = sample.split(titanic_data, SplitRatio = 0.75)
train_data <- subset(titanic_data, sample_data == TRUE)
test_data <- subset(titanic_data, sample_data == FALSE)

nrow(train_data)
nrow(test_data)

# Implement Model
# ----------------------------------------------------------------

rtree <- rpart(survived ~ ., train_data)
rtree
rpart.plot(rtree)
fancyRpartPlot(rtree)

# conditional parting Model
# ----------------------------------------------------------------

ctree_ <- ctree(survived ~ ., train_data)
plot(ctree_)


######################################### END ############################################



