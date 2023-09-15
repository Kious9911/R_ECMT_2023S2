## introduction to R
# this is an exercise of econometrics in R and the copyright is by Ani Katchova with some change by me
# the practice video is from bilibili BV1VJ411C7gd


# highlight the program you want to execute and click the "run" button

# set working directory to where csv file is located
setwd("F:/R_LEARNING/Basic")
getwd() # clarify the place of working file

# permanently change the path of storing packages 
.libPaths(c("D:/R-4.3.1/library",.libPaths()))
print("AccessToF")
.libPaths()

# package "Hmisc" includes a command named "describe" for descriptive statistics 
# remove.packages("Hmisc",lib= file.path("..."))
install.packages("Hmisc")
find.package("Hmisc")
library(Hmisc)

# ---- Read the data ----
mydata <- read.csv("F:/R_LEARNING/Basic/intro_auto.csv")
# if the data is in the same directory, include part of the path for simplicity as followings 
mydata <- read.csv("intro_auto.csv")

# ---- Display the detail of thee data -----
attach(mydata)
#list the variables 
names(mydata)
#show first lines of data 
head(mydata)
# show the row one to ten with all columns
mydata[1:10,]
# atother example 
mydata[1:8,5]
# view all the mpg data in "mydata"
mydata$mpg
data2 <- mydata[mydata$foreign == 1,]
data3 <- mydata[mydata$foreign ==1 & mydata$mpg>=20,]

# ----- descriptive statistics
myvars <- c("mpg","price")
summary(mydata[myvars])
# from packages "Hmisc"
describe(mydata[myvars])
# standard deviation 
sd(mydata[myvars])
length(mpg)

sort(make)
# frenquency table 
table(make)
table(make,foreign) # if it is domestic then = 0, otherwise = 1 for the foreign

# correlation among variables
cor(price,mpg)

# T-test for mean of one group. the null hypothesis is that the mean of mpg = 20  
t.test(mpg,mu=20)
# because the p-value which is 0.332 is greater than 0.05, so we conclude that we 
# cannot reject the null hypothesis 

# ANOVA for equality of means for two groups
anova(lm(mpg~factor(foreign)))
# the p-value is 0.0427, which is slightly below 0.05
# hence the conclusion that we have significant difference im mpg between domestic and foreign cars

# OLS regression with mpg being the dependent variable as well as weight, length and foreign being independent variables
olsreg <- lm(mpg~weight+length+foreign) # lm for linear model 
summary(olsreg)
# alternative way: summary(lm(mpg~weight+length+foreign))

#plotting the data 
plot(mpg~weight)
olsreg1 <- lm(mpg~weight) # estimate the linear model 
abline(olsreg1)
 
# redefining variables 
Y <- cbind(mpg)
X <- cbind(weight,length,foreign)
summary(Y)
summary(X)
olsreg <- lm(Y~X)
summary(olsreg)

# ------ linear regression ------
# clear all
rm(list=ls())
gc
# set a new data
setwd("F:/R_LEARNING/Basic")
mydata <- read.csv("regression_auto.csv")
# mydata <- read_excel("regression_auto.xls", sheet ="regression_auto")
View(mydata) # with capital V!!
attach(mydata)

# define variables
Y <- cbind(mpg)
x1 <- cbind(weight1)
X <- cbind(weight1, price, foreign)

# correlation among variables 
cor(Y,X) # mpg has a very high negative correlation with weight 

# plotting data on a scatter diagram 
plot(Y~x1, data = mydata) # only could plot with one independent variable 
# the plot also shows that the mpg has a negative correlation with weight 

#----- simple linear regression -----
olsreg1 <- lm(Y~x1)
summary(olsreg1)
# conclusion: the probability is significantly less than 0.05, 
# which means that we would say that the coefficient is significant
# the multiple R-squared is 0.65, which means that 65% of the variation is explained by the regression

# the confidence interval 
confint(olsreg1, level=0.95)
#in this confident interval, x_1's coefficient does not include zero
# this means that it's way from zero, and we have significantly different result from zero

# test for a joint significance of all the coefficients  
anova(olsreg1)
# because the probability of 5.935e-07 is less than 0.05, 
# so all the coefficient is jointly significantly different from zero

# plot the regression line 
abline(olsreg1)

# predicted values for dependent variable 
Y1hat <- fitted(olsreg1)
summary(Y1hat)
summary(Y)
plot(Y1hat ~x1)

# Regression residuals 
e1hat <- resid(olsreg1)
summary(e1hat)
plot(e1hat~x1)

# ----- multiple linear regression -----
olsreg2 <- lm(Y~X)
summary(olsreg2)
# holding all else constant, the independent variables price and foreign do not affect Y
confint(olsreg2,level=0.95)
# if the result does not include zero, then the coefficient is significantly different from zero

anova(olsreg2)
# joint significance of all the coefficients

Yhat <- fitted(olsreg2)
summary(Yhat)
summary(Y)
ehat <- resid(olsreg2)
summary(ehat)
# the mean of ehat is zero, which means the prediciton is quite good 
