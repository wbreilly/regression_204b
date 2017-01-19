#Lab 1 
#PSC 204B
#Winter 2017

#Expectations of lab
#1. Please be on time
#2. If you have questions, do not hesitate to raise your hand, we #are here to help you!
#3. Please do not check Facebook, email, reddit, etc. during lab #time, it is disruptive to your peers!

#Some basic review of R
#R is a command based program where you call functions to perform #your operations

#Technically, R programming is called object-oriented programming,
#where objects, such as vectors or matrices, contain data

#R functions manipulate objects, which contain the data, to the #programmer's whim

#R contains many built-in functions, as well as functions you can #download through libraries

#R is free so it is very popular

#Some basic operations

a = 1; a <- 1 
	#We are creating an object called a and assigning a value of 1 	#to it
	#You can use '=' or '<-' to create an object
	#I prefer '=' because it saves me a keystroke but you can use 	#whatever you would like, just be consistent!
	
a
	#this command calls the object 'a', which is 1 because we 		#assigned the value of 1 to it
	
a = b = 1
	#both a and b are assigned values of 1
	
a;b
	#R recognizes the semicolon as the start of another line, so we 	#can put multiple lines of code on a single line

a = c(1,2)
	#c() stands for the concatenate function (you will be using 	#this a lot), which basically creates a vector of values
a

###Structure of lab session###

#Lecture is theoretical while the lab is applied

#What you learn in lecture will be used in lab 

#Lab will be based more on the book so please do your assigned #reading every week!

#Let's delve into some lab material

#We will start with relationships between variables before moving #on to linear regression

#Assume we have collected some data on 4 variables and then we #have
#a correlation matrix
#Recall what a correlation matrix is

temp <- matrix(c(
   1,   .5,   .5,   .5,
  .5,    1,   .3,   .3,
  .5,   .3,    1,   .2,
  .5,   .3,   .2,    1), nrow = 4, ncol = 4, byrow = TRUE)
  
#The correlation matrix is created by using a combination of the #matrix() and c() functions

#Say we wanted to convert these correlations into covariances

#Recall that a correlation is a covariance divided by the two #standard deviations multiplied together

#If I gave you the standard deviations of each of the 4 variables, #you could compute the covariances by hand...

#...But that would take too long

#There is an R function to convert correlation matrices to #covariance matrices

#First install the necessary package
install.packages('MBESS') #may take a while, be patient

#Bring up the required package we just downloaded
library(MBESS)

#Use cor2cov() function to convert our correlation matrix
temp2 = cor2cov(temp, c(1,2,3,10))

######################################
#Pearson's Correlation 
######################################

#Pearson's correlation is the r coefficient you are all familiar 
#with

#Linear relationship between two continuous variables 

#To examine all the different types of correlations, we will work #with some simulated data

#Install necessary package
install.packages('MASS')

#Bring up the library
library(MASS)

set.seed(200)
	#setting the seed ensures that we all get the same result

temp3 = as.data.frame(mvrnorm(100, c(0,0,0,0), temp2, empirical = TRUE))
	#We are going to simulate data from the multivariate normal 	#distribution with mean = 0 for all 4 variables and a 			#covariance matrix equal to the temp2 cov matrix
	
	#The mvrnorm function is encased within as.data.frame(), which 	#will turn it into a data frame 

summary(temp3) 
	#let us examine a summary of our data frame

#cov() computes the covariance between all variables in a data #frame or two specific variables

cov(temp3) 
	#gives us a covariance matrix

cov(temp3[,1], temp3[,2]) 
	#gives us the covariance between V1 and #V2

#cor() does the same thing as cov(), but for correlations
cor(temp3) #correlation matrix

cor(temp3[,1], temp3[,2])	
	#gives us the correlation between V1 and #V2

#cor.test() tests the correlation between two continuous
	#variables. Additional arguments are available
	#This test includes test of significant for
	#correlation given the effect size and sample size
	#The default is to use Pearson's Product-Moment Correlation

cor.test(temp3$V2, temp3$V3)

cor.test(temp3$V2, temp3$V3)$p.value 
	#returns p-value of the test

########################################
#Point Biserial Correlation
########################################

#Point biserial correlation is a correlation between a categorical #variable (e.g. 'male' or 'female') and a continuous variable #(e.g. IQ)

#Let's add a dichotomous variable because it's necessary to #compute a point biserial correlation

temp3$male = 0
	#creating a variable called 'male' which is assigned a value 	#of 1

temp3[which(temp3[,4] > 0),'male'] = 1
	#the which() function selects observations
	#this command selects observations from V4 that are greater 	#than 0 and then turns the 'male' variable into a value of 1

#Let's compute a point biserial correlation

#We need the package ltm
install.packages('ltm')
library(ltm)

#We need to make sure that 'male' is being treated as a factor and #not a numeric variable
temp3$male = as.factor(temp3$male)

biserial.cor(temp3$V1, temp3$male) 
	#Assume that 0 is female and 1 is male. What does this 			#correlation tell us? 
# use cor.test to find p-value

########################################
#Phi Coefficient
########################################

#Recall that we need a 2x2 contingency table to compute a phi #coefficient

example =  matrix(c(19,54,60,52), nrow = 2, ncol = 2, byrow = TRUE)

install.packages('psych')
library(psych)

phi(example, digits = 3)

#The phi() function also takes a 1x4 vector of values...
phi(c(19,54,60,52), digits = 3)

########################################
#Beginning of Regression
########################################

#Last quarter you learned about ANOVA

#This quarter you will learn about regression, which is an extension of ANOVA

#Hopefully by the end of the quarter, you will recognize that most #of your statistical analyses is some form of regression (It's all #regression!)

mod1 = lm(V1 ~ V2, data = temp3) 	
	#Recall that with regression, we regress the dependent 			variable on the independent variable

summary(mod1) 
	#Gives us a summary of our model

mod1$coef 
	$Gives us the coefficients of our model