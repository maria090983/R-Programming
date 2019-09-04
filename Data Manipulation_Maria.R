# 2 ways to read file from Directory

# 1st Way - to set directory and then call/read file from that derectory

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\R Basic_Data Visualization & Manipulation\\HouseDB")
getwd()

housesalesdata <- read.csv('housesalesdata.csv')
housefeaturesdata <- read.csv('housefeatures.csv')

# 2nd way - to dirctly set file path and read file

h1<-read.csv("G:\\MARIA\\DSP Training\\R Programing\\Practice\\R Basic_Data Visualization & Manipulation\\HouseDB\\housesalesdata.csv")
h2<-read.csv("G:\\MARIA\\DSP Training\\R Programing\\Practice\\R Basic_Data Visualization & Manipulation\\HouseDB\\housefeatures.csv")

# Mearge/ Join Two files

housedata <- merge(housesalesdata, housefeaturesdata, by="ID")

View(housedata)
summary(housedata)

# We can write the merged dataset to a csv file with write.csv command
write.csv(housedata, file = "housedatacomplete.csv")


summary(housedata)


housedata$CarpetArea<-as.character(housedata$CarpetArea)
housedata$CarpetArea <- ifelse(housedata$CarpetArea=="na", NA, housedata$CarpetArea)
housedata$CarpetArea <- ifelse(housedata$CarpetArea=="Na", NA, housedata$CarpetArea)
housedata$CarpetArea <- as.numeric(housedata$CarpetArea)

# Imputing missing values

housedata$CarpetAre[is.na(housedata$CarpetArea)]<-mean(housedata$CarpetArea,na.rm = T)
View(housedata)

summary(housedata)

housedata$CarpetAre[is.na(housedata$CarpetArea)]<-round(mean(housedata$CarpetArea,na.rm = T),1)

mean(housedata$DistFromStreet, na.rm = T)
sd(housedata$SalePrice)
sd(housedata$DistFromStreet, na.rm=T)

cor(housedata$BuiltYear, housedata$SalePrice)
cor(housedata$DistFromStreet, housedata$SalePrice, use = 'complete.obs')


#See data distribution of numeric variables using hist command

options(scipen=999)
hist(housedata$SalePrice)
boxplot(housedata$SalePrice,horizontal = T)


# See distribution of categorical variable using table or prop.table command

table(housedata$SaleType)

# return % values
prop.table(table(housedata$SaleType))

g1<-barplot(table(housedata$SaleType))
g2<-barplot(prop.table(table(housedata$SaleType)))

#See data distribution using hist command with other arguments
#Turn off scientific notation. To turn it on, set scipen=0


options(scipen = 999)
hist(housedata$SalePrice, main="House Price Histogram", 
     xlab = "Sale Price", ylab = "Count", labels = T, 
     xlim = c(0, 500000), ylim=c(0,500), 
     include.lowest = T, axes = T, col="Purple")


options(scipen=0)
# See other statistics like mean, median, standard deviation, correlation
# Notice that we have NA in our data.
# na.rm=T will ignore NAs in your data and give you the output
# If you do not use na.rm=T, it will consider NA and
# and will not be able to give you the output value because
# any number + NA = NA

mean(housedata$DistFromStreet,na.rm = T)
median(housedata$DistFromStreet,na.rm = T)
sd(housedata$DistFromStreet,na.rm = T)

cor(housedata$DistFromStreet,housedata$SalePrice,use = "complete.obs")

# Throughs error
cor(housedata)

# Filter out only character values
character_df<-Filter(is.character,housedata)
names(character_df)

# Filter out only numeric values
numeric_df<-Filter(is.numeric,housedata)
names(numeric_df)

cor(numeric_df, use = 'complete.obs')
plot(housedata$Fireplaces, housedata$SalePrice)
cor(housedata$Fireplaces, housedata$SalePrice)


# Finding correlation of all numeric variables in the dataset
# if we have NOT filtered the numeric variables only,
# then cor(housedata) would have thrown error because
# it cannot find correlation of factor variable
# So we filtered only numeric variable and then ran the cor() function
# Also, we are interested in finding correlation not only of numeric variable
# but also we are not interested in ID column

# So we use -1 in order to NOT consider it

cormat<-cor(numeric_df[,-1],use='complete.obs')
cormat
write.csv(cormat, 'cormatrix.csv')

# U can also plot a few variables with the below code
plot(numeric_df[,2:6])
plot(numeric_df[,c(2,4,6,8)])

# Study your data
# Notice that some categorical variables got imported as integer/numeric
# Like, HouseType & HouseCondition
# So change it to factor variable

class(housedata$DistFromStreet)
housedata$DistFromStreet<-as.factor(housedata$DistFromStreet)
class(housedata$DistFromStreet)
housedata$DistFromStreet<-as.integer(housedata$HouseCondition)
class(housedata$HouseCondition)

housedata$HouseType<-as.factor(housedata$HouseType)


# Also we will not need variable ID 
# So we can remove it from our dataset
# Index of ID is 1
# So we use -1 to remove that variable

housedata <- housedata[,-1]
View(housedata)


# Missing value analysis
# Check for Which columns contain NA values
#colSums()

na.cols <- which(colSums(is.na(housedata)) > 0)
na.cols

sort(colSums(sapply(housedata[na.cols], is.na)), decreasing = TRUE)

# Check for NAs percentage in columns

NAcount <- colSums(sapply(housedata[na.cols], is.na))
NAcount
sort((NAcount*100/nrow(housedata)), decreasing = TRUE)

# ------ OR---(find missing(NA) value veriables) ------------

!colSums(is.na(housedata))

unique(housedata$DistFromStreet)!=0
unique(housedata$CarpetArea)!=0

colSums(is.na(housedata))
is.na(housedata$DistFromStreet)

# rowSums

na.cols <- which(rowSums(is.na(housedata)) > 0)
na.cols
# ----------------------------------------------------------

library(Amelia)
missmap(housedata)

# Missing value treatment
# Option 1: For numeric variables, fill in with median
 
housedata$DistFromStreet <- ifelse(is.na(housedata$DistFromStreet), 
                                   median(housedata$DistFromStreet, na.rm = T),
                                   housedata$DistFromStreet)
colSums(is.na(housedata))
typeof(housedata$CarpetAre)
housedata$CarpetArea <- ifelse(is.na(housedata$CarpetArea), 
                                   median(housedata$CarpetArea, na.rm = T),
                                   housedata$CarpetArea)

# See summary after treating missing values with median
# You will notice that NAs are gone
View(housedata)
summary(housedata)
missmap(housedata)

# -------- OR -----
library(DMwR)
house_data <- centralImputation(housedata)
colSums(is.na(house_data))
missmap(house_data)
View(house_data)

# aggregate() is only to view mean/median of CarpetArea, grouped by HouseType
#aggregate(object/dataset name,groupby variable(list of grouping elements),FUN)

aggregate(data = housedata, housedata$CarpetArea ~ housedata$HouseType, mean, na.rm = TRUE)

# Create a vector of average carpet area
# After running this syntax, run ave_carepetarea and see what goes into ave_carepetarea

ave_carepetarea <- ave(housedata$CarpetArea, housedata$HouseType, FUN = function(x) median(x, na.rm = TRUE))

# You will see it has created a vector of average of sales price
# by Each type of house (number of elements in this vector is same as
# number of observations in the data set)
ave_carepetarea

# if the values in CarpetArea is NA, 
# replace with the values from ave_carepetarea
# else replace with the same value of CarpetArea
housedata$CarpetArea <- ifelse(is.na(housedata$CarpetArea), ave_carepetarea, housedata$CarpetArea)
summary(housedata)

boxplot(housedata$SalePrice)

# Distribution of sale price by Zone, Landmark, Sale Type, House Type
library(RColorBrewer)
boxplot(housedata$SalePrice ~ housedata$Zone, data=housedata, horizontal = FALSE,
        xlab="Zone", ylab="Sale Price", main="Zonewise Sale Price",
        col=brewer.pal(5, "Set3"))

boxplot(housedata$SalePrice ~ housedata$Landmark, data=housedata, horizontal = FALSE,
        xlab="Zone", ylab="Sale Price", main="Landmark-wise Sale Price",
        col=brewer.pal(5, "Set3"))

boxplot(housedata$SalePrice ~ housedata$SaleType, data=housedata, horizontal = FALSE,
        xlab="Zone", ylab="Sale Price", main="Sale Type wise Sale Price",
        col=brewer.pal(5, "Set3"))

boxplot(housedata$CarpetArea ~ housedata$HouseType, data=housedata, horizontal = FALSE,
        xlab="House Type", ylab="Carpet Area", main="Carepet Area by House Type",
        col=brewer.pal(5, "Set3"))

# A few examples of Feature engineering
# Create Age of building as on current year
# Find the current year from Sys.Date()
currentyear = format(Sys.Date(), "%Y")
currentyear
typeof(currentyear)

# You will notice that currentyear is coming up as a string
# so convert currentyear into numeric
currentyear <- as.numeric(currentyear)
typeof(currentyear)

# Find the difference between current year and builtyear
# to calculate the age of the building
housedata$buildingage <- currentyear - housedata$BuiltYear
housedata$buildingage

# Create Age of building when sold
housedata$age_when_sold <- housedata$YearSold - housedata$BuiltYear
housedata$age_when_sold

# Bin distfromstreet into 10 segments - create a new variable
# You can create binning using ifelse statement
# or if you have many bins to be created use cut()
housedata$binneddist <- cut(housedata$DistFromStreet, 
                            c(0,10,20,30,40,50,60,70,80))
table(housedata$binneddist)
prop.table(table(housedata$binneddist))



