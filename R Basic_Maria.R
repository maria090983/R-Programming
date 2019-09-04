
# Numeric Datatype
x = 10.1
x

# Integer Data Type
x = as.integer(x)
x

# Passed Integer val
as.integer(10.9)

# Complex Data Type
x = 1 + 2i
x

sqrt(-1)

# Logical Datatype
a = 5; b = 10
c = a>b
c

# Store String
x = "Hello"
x

# ---------- Vector Creation --------------
v <- c(1,2,3,4)
v

# Numeric Vector
num_V <- c(1.3,4,5.3,7)
num_V
v1 <- 1:100
v1
# Operations on numeric Vectors
op <- v1+2
op
op1 <- v1-4
op1

# Logical Vector
log_V <- c(T,F,T,F,T)
log_V

# Character Vector
char_V <- c("abc","xyz","lmn")
char_V

# Creating a list with 4 different types of values
stud_info <- c(name="Moses", age=6, gender="Male",height=4.3, loveicecream=T)
stud_info

# Creating a list with names & age
name <- c("sonu", "alen","moses","angel","sam")
age <- c(21,11,6,2,1)
detail <- c(name,age)
detail

# Creating a list with a single name and marks
my_marks <- c(45,67,84,64)
stud_detail <- c(name="alen",allmarks=my_marks)
stud_detail

# Atomic Vector
x <- "abc"
typeof(x)

x <- 10.3
typeof(x)

x <- 23L
typeof(x)

x <- TRUE
typeof(x)

# ---- Vector Manipulation---

# Addition / Subtraction/ Multiplication/ Division
V1 <- c(12, 34, 45, 63, 43)
V2 <- c(34, 24, 53, 16, 74)

add <- V1 + V2
add

sub <- V1 - V2
sub

mul <- V1 * V2
mul

div <- V1/V2
div

# ---- Vector Functions ------

max(V1)
min(V2)
sum(V1)
mean(V2)
median(V1)
range(V2)
var(V2)
cor(V1,V2)
sort(V1)
rank(V2)

# Converting as a factor
jobs <- c(rep("Data Analyst",20),rep("IT Professional",10),rep("Data Engg",15))
summary(jobs)

jobs <- as.factor(jobs)
summary(jobs)

# ------ List Operations ------

# List contains Differant type of data
list.data <- list("abc",'a',12.4,24L)
print(list.data)

# Naming list elements
l1 <- list(even = seq(2,10,2), odd = seq(1,10,2), lang = c('R','Python','Java'))
l1

# Combining list element
l2 <- list('a','b','c')
comb <- c(l1,l2)
comb

# Converting list to vector
l1 <- list(1:10)
l1

v1 <- unlist(l1)
v1

typeof(v1)

# Create 2 vectors of different lenths
v1 <- c(2,3,5)
v2 <- c(12,22,1,44,22,65,18)

# Take these vectors as input to the array
result <- array(c(v1,v2),dim = c(4,4))
result              # Accessing array elements

# Naming columns and rows
col_names <- c('c1','c2','c3','c4')
row_names <- c('r1','r2','r3','r4')

# Take these vectors as input to array
result <- array(c(v1,v2),dim = c(4,4),dimnames = list(row_names,col_names))
result

# Accessing array elements according to input

result[3,]    # prnint 3rd row of array
result[,4]    # prnint 4th column of array
result[2,3]   # prnint 2nd row and 3rd column element of array

# Calculations across array element
print(result)

result <- apply(result, c(2),sum)
result

# --------- Subsetting vectors in R --------

# Using positive integer
x <- c(1,3,4,5,6,7,5,8,9)
x
x[7]
x[c(3,4)]
x[c(3,3)]
x[c(12.1,11,6)]
x[c(2.1,2.9)]

# Using negative integers
x[-8]        # skip 8th element
x[-c(2,3)]   # skip 2nd & 3rd element
x[c(-3,-4)]

# Using logical operator
x[c(T,T,F,T,F,F)]

x[x<3]
x[x>3]

# with nothing (usful with 2 or higher dimesional object )
x[]

# with Zero (useful for generating test data or creating empty object)
x[0]

# ------- Matrices and Data Frames -----------

# Create matrix Row wise
m <- matrix(c(5:16),nrow = 3,byrow = T)
m

# Creates 10 x 12 numeric matrix
my_mat <- matrix(1:20,nrow = 10,ncol = 12)
# OR
my_mat <- matrix(1:20,10,12)
my_mat

# Naming columns and rows
col_names <- c('c1','c2','c3','c4')
row_names <- c('r1','r2','r3')

r1 <- matrix(c(5:16),nrow = 3,byrow = T,dimnames = list(row_names,col_names))
r1

# Accessing elements of matrix
print(r1)
print(r1[1,2])

# Creates 3 X 3 multi-dimensional matrix
Distance <- c(152,252,623,163,443,373)
Destination <- c("Pune","Banglor","Chennai")
Source <- c("Dehli","Kolkata","Mumbai")
my_matrix <- matrix(Distance,3,3,byrow = T,list(Source,Destination))
my_matrix

my_matrix[1,3] # 1st row, 3rd column of matrix
my_matrix[2,] # 2rd row of matrix 
my_matrix[1,2:3] # 1st row of columns 2 & 3

# Arithmetical operations on matrix

m1 <- matrix(c(1:4),nrow = 2)
m1
m2 <- matrix(c(7:10),nrow = 2)
m2

# Addition
add <- m1 + m2
add      

# subtraction
sub <- m1 - m2
sub

# Multiplication
mul <- m1 * m2
mul

# Division
div <- m1 / m2
div

# -------- Data Frames ------

# Creating data frame
Sub <- c('Phy','Math','Chem','Bio','Eng')
Per <- c(80,98,78,96,90)
stud_df <- data.frame(Sub,Per)
stud_df

# Get structure of Data Frame
structure(stud_df)
str(stud_df)

# Summary of Data Frame
summary(stud_df)

#-----------
Age <- c(23,34,21,12,67)
Name<-c("Mohan","Leslie","Raj","Gayathri","Robert")
Income <- c(20000,35000,19000,12000,42000)
Ownshouse <- c(TRUE,FALSE,TRUE,TRUE,FALSE)

my_df <- data.frame(Age, Name, Income, Ownshouse)
my_df

colnames(my_df)
rownames(my_df)
nrow(my_df)
ncol(my_df)

help(array)
col_bind <- cbind(Age,Name,Income)
col_bind

row_bind <- rbind(Age,Name,Income)
row_bind

#------------ Logical Statements ------------

# if Statement
x <- 24
if(x > 0)
{
  print("This is positive number")
}

# if-else Statement
x <- -9
if(x >= 0)
{
  print("This is non Negative number")
}else 
  {
    print("This is Negative number")
  }

# Nested if-else Statement
x <- -9
if(x < 0)
{
  print("This is Negative number")
}else if(x > 0) 
  {
    print("This is Positive number")
  }else
  {
   print("This is zero")
  }

# for Loop Statement
x <- c('a','b','c','d')
for(i in 1:4)
{
  print(x[i])
}

# while Statement
# compute the square of numbers till 50
i <- 1
while (i <= 50) 
  {
    print(i * i)
    i = i +1
}

# switch Statement
x <- switch (
              3,"1st","2nd","3rd","4th"
            )
print(x)

# which Statement
my_data <- airquality
summary(my_data)
filter_data <- my_data[which(my_data$Wind>12),]
filter_data

# subset Statement
attach(iris)
str(iris)
new_data <- subset(iris, Sepal.Length >= 4 | Sepal.Length < 5, select = c(1,2))
new_data
dim(iris)

# -------------- Function() -----------------

my_fun <- function(i)
{
  print(i^2)
}
my_fun(999)

my_fun1 <- function(i)
{
  for(i in 1:30)
  {
    i = i + 10
    next
  }
  print(i)
}
my_fun1(10)

system.time(my_fun1(10))

# Variable Scope
y <- 3

myfunction <- function(x){
  y <- 2
  y^2 + g(x)
}

g <- function(x){
  x*y
}

# what is the value of myfunction(x)?
myfunction(6)

# ------------ Handling Missing Values ------------

# Identifying missing values
# Ex1
x <- c(3, 4, NA, 5, 6, NA, 7,NaN,2)
is.na(x)
!is.na(x)
is.nan(x)

# Ex2
data_withNA <- c(1:5,NA,NA,8:10)
data_withNA
is.na(data_withNA)
which(is.na(data_withNA))

#Convert NA to 0
ifelse(is.na(data_withNA),0,data_withNA)

# Missing value Imputation

library(Hmisc)

# --------- Apply Family ----------

# Purpose : avoid explicit uses of loop constructs
# Syntax : apply(x,margin,function)
# x: array or Matrix; margin=1: manipulation is performed on row, margin=2: on column

mymatrix <- matrix(1:30,5,6)
mymatrix

# row & column wise standard deviation/mean computation
apply(mymatrix, 1, sd)  # row wise
apply(mymatrix, 2,mean) # column wise

# lapply()
# Used for dataframe,list objects, o/p: list
# Syntax: lapply(x,function)

mylist <- list(1:20)
mylist
lapply(mylist, median)
class(mylist)

# sapply()
# Syntax: sapply(x,function) : I/p: Vector  o/p: vector

s <- sapply(list(1:20,5:15,1:10), median)
s
class(s)

# tapply()
# tapply() is useful when vector is broken into groups or categories
# 1st arg is the data, 2nd arg is the grouping, 3rd arg is the function

tapply(mtcars$mpg, mtcars$gear, mean)

# Syntax: tapply(x,index,function=NULL)
# x: object,usally vector; index: list containing factor

age <- c(14,23,52,62,15)
location <- c("urban","rural","urban","urban","rural")
tapply(age, location, mean)

# vapply()
# similar to sapply, but it requires u to specify what type of data u r expecting
# Syntax: vapply(x,function,function.value) 
# function.value: specify type of data

A <- c(1:9)
B <- c(1:12)
C <- c(1:15)
my_list <- list(A,B,C)
my_list
vapply(my_list,sum,numeric(1))

# throws error : if fun return more than 1 numeric values
vapply(my_list, function(x) x+2, numeric(1))


# ------ Sampling ---------------

data1 <- iris
summary(data1)
head(data1)
data1[sample(nrow(data1),5),]   # retrieves 5 rows randomly

# OR

library(datasets)
data(airquality)
data2 <- airquality
summary(data2)

library(caret)
data = airquality
dim(data)

indexes = sample(1:nrow(data), size = 0.2 * nrow(data))
indexes

# ------ Split data -----------

test = data[indexes,]
test
dim(test)
train = data[-indexes,]
train
dim(train)

library(MASS)
data(AirPassengers)
d1 <- AirPassengers
d1
d1_frame <- data.frame(d1)
d1_frame
summary(d1_frame)

library(MASS)
data()
data("airquality")
mydata <- airquality
mydata_frame <- data.frame(mydata)
head(mydata)
tail(mydata)

summary(mydata)

mean(mydata$Wind)
median(mydata$Wind)
sd(mydata$Wind)
var(mydata$Wind)

cor(mydata)

# ------------- Date Formating ---------------
#Initial Date is 1/1/70

dt <- 365
class(dt) <- "date"
dt

dt1 <- as.Date(1000, origin = as.Date("1980-03-31"))
dt1

dt2 <- as.Date(-1000, origin=as.Date("1980-03-31"))
dt2

#Get Year as four digits (as character or numeric)
format(dt1,"%Y")    # Character
as.numeric(format(dt1,"%Y"))   # Numeric

#Get year as 2 digits
format(dt1,"%y")

#Get month (in character or numeric)
format(dt2,"%m")
as.numeric(format(dt2,"%m"))

#Get month (in character)
format(dt1,"%b")
format(dt2,"%B")

#Get week number (in character)
format(dt1,"%w")
format(dt1,"%W")

months(dt1)
weekdays(dt2)
quarters(dt1)

dt <- as.Date("01/01/1983",format = "%d/%m/%Y")
dt

my_Var <- "21Feb2013"
class(my_Var)
my_Date_Var <- as.Date(my_Var, format = "%d%B%Y")
class(my_Date_Var)
my_Date_Var

my_var1 <- "01-05-2016"
class(my_var1)
my_date_var1 <- as.Date(my_var1, format="%d-%m-%y")
class(my_date_var1)
my_date_var1

# Date operations
dt1 <- as.Date("1/1/2001", format = "%d/%m/%Y")
dt2 <- as.Date("2/1/2002", format = "%d/%m/%Y")

dt2 - dt1
dt2 > dt1
dt2 < dt1
dt2 == dt1

#Date Operations
d1 <- as.Date("1980/1/1")
d2 <- as.Date("1982/1/1")
#Specify Start Date, End Date & Intervam
seq(d1, d2, "month")
seq(d1, d2, "day")
seq(d1, d2, "week")

seq(d1, d2, "2 month")
seq(d1, d2,"52 week")
seq(d1, d2, by = "10 week")

# --------- central Imputation -----------
# centralImputation() fills in any NA value in all columns of a data frame with the statistic of centrality

setwd("G:\\MARIA\\DSP Training\\R Programing\\Practice\\Linear Regression\\Titanic DB\\Titanic_Dataset")
getwd()

# Ex 1
my_data_imputation <- read.csv("train.csv")
dim(my_data_imputation)
summary(my_data_imputation)
colSums(is.na(my_data_imputation))
sapply(my_data_imputation,class)

library(DMwR)
impute_data <- centralImputation(my_data_imputation)
summary(impute_data)
colSums(is.na(impute_data))
View(impute_data)

# Ex 2
data(algae)
sapply(algae,class)
clean_data <- centralImputation(algae)
summary(clean_data)
colSums(is.na(clean_data))
View(algae)

# ------- RANDOM NUMBERS --------

b <- rnorm(10)  # Standard Normal Distribution
b
c <- rf(10,df1 = 5, df2 = 11)     # Skewed   df(degree of freedom)
c

hist(b)
hist(c)

# ---- Sorting, Ordering, Ranking -----

sales <- c(100,50,75,150,200,25)
sales
ranked <- rank(sales) 
ranked
sorted <- sort(sales) 
sorted
ordered <- order(sales)
ordered

# Make a data frame using the four vectors
view <-data.frame(sales,ranked,sorted,ordered)
view

#Using %in%
all_sports <- c("rugby","cricket","badminton","football","baseball","tennis","basketball")
rank(all_sports)
sports_i_like <- c("football","cricket","basketball","baseball")

which(sports_i_like %in% all_sports)
sports_i_like[which(sports_i_like %in% all_sports)]

mydata <- airquality
filterdata <- mydata[which(mydata$Wind<10),]
filterdata

# ----- Using substr -----
word = "Apple"
substr(word, start = 1, stop=2)

# ----- Replace substr characters -----
replacewith <- "XX"
substr(word, start = 1, stop=2) <- replacewith
word

# Summarizing at each factor 
a <- c(1,1,1,1,2,2,2,2,2)
b <- c(10,12,15,12,NA,30,42,38,40)

s<- split(b,a)
s
lapply(s, mean)

# Summarizing at each factor, with missing value treatment

lapply(s, mean, na.rm=TRUE)

