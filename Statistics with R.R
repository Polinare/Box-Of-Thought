#set name of the file and save to desired working directory
megaguide
#set working directory

#get csv. file using " <- read.csv("filename")
megaguide <- read.csv("sample1.csv")
megaguide

#to check on items in each column, use "$NAME" or specification of what column
megaguide$NAME
megaguide$SEX
megaguide$ORDER
megaguide$GRADE
megaguide$AGE
megaguide$SECTION

#to add columns with data, use "<- c("data to be inputted")
megaguide$LASTNAME <- c("Cruz", "Pena", "Lui", "Roa", "Lim", "Canedo", "Ramos", "Gerona", "Ocon", "Cu")
megaguide
#to add a column of data with just numerals, "<- c(data to be inputted)
megaguide$NUMBER <- c(3,3,3,3,3,3,3,3,3,3)
megaguide

#to locate a certain value in a specific row, use "[]"
megaguide$NAME[3]

#to find the mean and median, use function, "mean(name of file and column)" and "median(name of file and column)
mean(megaguide$ORDER)
median(megaguide$ORDER)

#to get the mode, first call the library "modeest" using 'help.start(modeest)'
#next is to assign values as numericals using "as.numeric(name of column)"
#And to determine the mode, input "mfv(as.numeric(name of column))
'help.start(modeest)'
library("modeest")
as.numeric(megaguide$SEX)
mfv(as.numeric(megaguide$SEX))

#to get the minimum, maximum and range, use functions "min" "max" and "range" respectively then (name of column)
min(megaguide$GRADE)
max(megaguide$GRADE)
range(megaguide$GRADE)

#to get the standard deviation, use the function "sd(name of column)"
sd(megaguide$ORDER)

#to get the variance, use the the function "var(name of column)"
var(megaguide$ORDER)

#to get the summary on the values of the file, use the function "summary(name of code file)"
summary(megaguide)

#to get quantile values, use function "quantile(name of column, percentage)"
quantile(megaguide$AGE, 0.5)
quantile(megaguide$AGE, 0.75)
quantile(megaguide$AGE, 0.25)

#to get the box-whiskers plot diagram, use the function "boxplot(name of column)"
boxplot(megaguide$ORDER)

#first call the library mosaic using the function 'help.start(mosaic)'
#to get the zscore of a specific column, use the function "zscore(name of column)"
'help.start(mosaic)'
library(mosaic)
zscore(megaguide$GRADE)

#to get a histogram figure of the a certain group of data, first call the data
#use the function "hist(name of column)"
AirPassengers
hist(AirPassengers)

#to set breaks in the figure, use the function "seq(bottom limit,top limit, by = what degree of increments)"
#then set to a name that is easier to type using "<- seq(bottom limit,top limit, by = what degree of increments)"
seq(100,650,by = 50)
breaks <- seq(100,650,by = 50)
breaks

#to set the default cutting, use the function "cut(Name of file, sequence name set, right=FALSE)"
#then give the cut a shorter name using the function "<- cut(Name of file, sequence name set, right=FALSE)"
cut(AirPassengers, breaks, right=FALSE)
Interval <- cut(AirPassengers, breaks, right=FALSE)
Interval

#to present the values on each interval, use the function "table(name of cut)"
#then give the table a shorter name using the function "<- table(name of cut)" 
table(Interval)
Allocation <- table(Interval)
Allocation

#to present a normal probability curve, start by setting the x and y values
#to set the x parameters, use the function "x <- seq(bottom limit,top limit, by = what degree of increments)"
#to set the y parameters, use the function "y <- dbinom(x,total size,probability of each experiment)
#check the figure using the function "plot(x,y)"
x <- seq(0,100,5)
x
y <- dbinom(x,100,0.5)
y
plot(x,y)

#to get the zscore, use the function "pnorm(x,mean,standard deviation)
pnorm(0,0,1)

#to get the zscore without the need of conversion
pnorm(11.25,11.5,2)

#BIVARIATE LINEAR REGRESSION
#Start with calling in data "cars"
cars

#to get the first 6 rows, use the function "head(name of data)"
head(cars)

#assign an independent (x) and dependent (y) variable
#assign a column to the corresponding variables through "x <- (nameofdata$column)
#then get the plot figure using the function "plot(x,y)"
x <- cars$speed
y <- cars$dist
plot(x,y)

#next is to build both the linear and exponential model
#use the funcion "lm(dependent variable ~ independent variable, data = Name of data)"
#as for exponential, simply add log to the independent variable, "lm(log(dependent variable) ~ independent variable, data = Name of data)"
lm(dist ~ speed, data = cars)
lm(log(dist) ~ speed, data = cars)
#then assign them shorter names using the function "NAME <- (the formula)"
Linear <- lm(dist ~ speed, data = cars)
Exponential <- lm(log(dist) ~ speed, data = cars)
#do remember that the function "lm" refers to "linear model" and "~" refers to "in terms of..."

#MULTIVARIATE REGRESSION
#start with calling the complex set of data "mtcars"
mtcars

#to get the first 6 rows, use the function "head(name of data)"
head(mtcars)

#to build the model, use the function "lm(dependent variable) ~ independent variable 1+2+3+4+..+n, data = Name of data)
lm(disp ~ hp+drat+wt, data = mtcars)

#LINEAR REGRESSION AND CORELATION
#Corelation is a range of 1 to -1 proving the data to be a better model if the Correlation value is closer to 1 or -1
#Start by calling the data
megaguide <- read.csv("sample4.csv")

#Calculate for all the needed values and apply to the following formulas
#to get the sums of each column and add an additional column in such, use the function "
#never forget to set the value of n using the function "n = number of rows"
x <- (megaguide$carat)
y <- (megaguide$price)

n = 9
sumX <- sum(x)
sumX2 <- sum(x^2)
sumY <- sum(y)
sumY2 <- sum(y^2)
sumXY <- sum(x*y)

#then incorporate into Corelation Formula
c = (n*sumXY-sumX*sumY)/(sqrt(n*sumX2-sumX^2)*sqrt(n*sumY2-sumY^2))

#then incorporate into the regression formula getting the "y-hat" intercept and coefficient
#then add the function "name of linear model$coefficients" to get the decimal points for a more accurate answer
lm(price ~ carat, data = megaguide)
linearmodel1 <- lm(price ~ carat, data = megaguide)
linearmodel1$coefficients

#then to get the differences or the relative error, one needs to incorporate all x's into the "y-hat" formula
#this can be done through the making a new set of X rows and Y rows, where the y is now the "y-hat" values
#use the function "data.frame(name of new column = name of file$old name of column)"
#and then use the function "predict(linearmodel1, new data = new name of column)"
#Lastly get the sum of the differences using the function "sum(old y values - new set of y values)"
data.frame(carat = megaguide$carat)
caratL <- data.frame(carat = megaguide$carat)
data.frame 
predict(linearmodel1, newdata = caratL)
pricesL <- predict(linearmodel1, newdata = caratL)
sum((megaguide$price-pricesL)^2)

#EXPONENTIAL REGRESSION
#will primarily use up the same set of values that the linear model does knowing it can be derived from such
#the only difference will be that the y value will be utilizing the log form of such with the function "log(name of data$name of column)"
lm(log(price) ~ carat, data = megaguide)
expomodel1 <- lm(log(price) ~ carat, data = megaguide)
expomodel1$coefficients

#next is to get the relative error
#same set of steps as that of the linear utilizing the exponential coefficients and intercepts
#however one needs to reconvert to the initial formula by using the function "exp(predict(name of exponential model, newdata = name of new x data column))"
data.frame(carat = megaguide$carat)
caratE <- data.frame(carat = megaguide$carat)
exp(predict(expomodel1, newdata = caratE))
pricesE <- exp(predict(expomodel1, newdata = caratE))
sum((megaguide$price-pricesE)^2)
#the value with the lesser relative error becomes the better suited model!

#HOW NOT FAIL 
#first step is to set the working directory using the function "setwd("name of location")
#note that the name of location will be found on the top bar of your R Script after saving it to a location
#remember that the name of the location starts from "USER, all the way before the \name of file"
#and all "\" must be switched or changed to "/"


#Item number 1 (30 POINTS)
#This item is solely a single column with over 10000 values
#Here, you will simply need to understand the concept of the Normal Distribution Curve 
range(name of file$name of column)
mean(name of flie$name of column)
sd(name of file$name of column)
#these are the preliminary data for you to understand how the normal curve is structured

#next is to form or change the given histogram from the set of data based on the range, mean and sd acquired
hist(name of file$name of column)
seq(lowest end of the data based on the range, highest end of the data based on the range, by = standard deviation)
cut(name of data$name of column, name assigned to the sequence, right=false)
#dont forget to the call the cut so as to assure yourself that the sequences have been applied, as well as a bracket to the left and a parenthesis to the right
table(name of cuts)
#this table will give you a summary of all the values found within each interval, it should present you with something normally distributed

#last step is to get the percentages of the values from one 1sd, 2sds and 3sds respectively
1sd = (sum of the values found within the 2 intervals from the middle)/total number of values
2sd = (sum of the values found within the 4 intervals from the middle)/total number of values
3sd = (sum of the values found within the 6 intervals from the middle)/total number of values
#a good thing to note here is that by Empirical Rule, percentages from 1sd, 2sds, and 3sds from the mean are 68%, 95%, 99% respectively
#last thing to do is to know how to properly conclude that the data given is normally distributed, if it is


#Item number 2 (20 POINTS)
#this is known as the "pamigay puntos" item that focuses on Pnorm values
#be sure to remember that the pnorm value one gets is the z-value encompasing all points to the left of that point
pnorm(x, mean, standard deviation)
#it is advised that one brings their own Z-table, as discussed by sir


#Item number 3 (50 POINTS)
#this item focuses on finding the most suitable model to explain the set of dots possibly plotted
#here you will be given 6 columns, instructions will be given as to which is the x and y values
#after given these x and y values, one is then instructed to get either linear only, exponential only or both 
#the multivariate in terms of numerous x's will also be applied here

#Applicable to All, get first the preliminary values of 
sumX
sumY
sumXY
sumX2
sumY2

r= (n*sumXY-sumX*sumY)/sqrt(n*sumX2-sumX^2)*sqrt(n*sumY2-sumY^2)
r^2
cor.test(~ dependent variable + independent variable, data = name of file, type = "pearson")

lm(dependent variable ~ independent variable, data = name of file)
name of linear model$coefficients
summary(the name given to the linear model)
#the summary makes it relatively easier to actually get the Corelation Coefficient or the "r^2"
#the r^2 value will be the "Multiple Regression" value in the summary you run
#using the function "$coefficients" makes the values that result from it more accurate by presenting decimals

data.frame(name of column = name of file$name of old column)
predict(name of linear model, newdata = name of data.frame)
sum((old set of y values-new set of y values)^2)

lm(log(dependent variable) ~ independent variable, data = name of file)
name of expo model$coefficients
summary(the name of the expo model)``

data.frame(name of column in the new data frame = name of file$name of old column)
exp(predict(name of expo model, newdata = name of data frame))
sum((old set of y values-new set of y values)^2)

lm(dependent variable ~ independent variable 1 + independent variable 2 + independent variable 3, data = name of file)
name of multivariate model$coefficients
summary(the name of multivariate model)

data.frame(name of column in the new data frame = name of file$name of old column)
predict(name of multivariate model, newdata = name of data frame)
sum((old set of y values - new set of y values)^2)
#be careful and note that this process can be repeated numerous times
#being able to solve one instance will assure you being able to solve the rest
#lastly, the best model can only be achieved by having the least relative error

#LETS PASS!













