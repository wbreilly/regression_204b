#Tim Banh & Ryan Hutchings
#PSC 204B
#Winter 2017
#Lab 3

#---------------------------
#Packages we'll use today
#---------------------------

library(MASS)
library(MBESS)

#Make a little data for us to work with

m1 <- matrix(c(
   1,   .5,   .5,
  .5,    1,  .25,
  .5,  .25,    1), nrow = 3, ncol = 3, 
	byrow = TRUE)
	
m2 <- cor2cov(m1, c(2, 1.8, 2.3))
set.seed(123)

d <- as.data.frame(mvrnorm(200, c(0,0,0), 
	m2, empirical = TRUE))
	
colnames(d) <- c('y','x1','x2')

#---------------------------

#Do we have what we asked for?

cor(d)

round(sapply(d, mean), 4)
	#basically

#What's R^2 for the individual correlations?	
cor(d)^2

#Therefore R^2 for a regression model
#between y and either x1 or x2 should
#be equal to .25

#Let's make a regression model between y and x1 and examine the R^2

mod1 = lm(y ~ x1, data = d)

summary(mod1)$r.squared

#Make a regression model between y and x2 and examine the R^2

mod2 = lm(y ~ x2, data = d)
summary(mod2)$r.squared

#We get what we are expecting...

#Thoughts? 

#Questions:
#What is the maximal R^2 value we could expect
#in a regression of y on both x1 and x2?

#What is the minimal R^2 value we could expect
#in a regression of y on both x1 and x2?

#R^2 can be no smaller than the largest R^2
#between y and either x1 or x2 at one time,
#and no larger than the sum of R^2 for y
#with x1, and y with x2.

#min value
max(c(summary(lm(y ~ x1, data = d))$r.square,
	summary(lm(y ~ x2, data = d))$r.square
	))
	
#max value
sum(c(summary(lm(y ~ x1, data = d))$r.square,
	summary(lm(y ~ x2, data = d))$r.square
	))

#Recall the formula for standardized coefficients
#r(y1) - r(y2)*r(12)/ 1 - r^2(12)
#y = dependent variable, 1 = predictor 1, 2 = predictor 2

b1 = (cor(d$y,d$x1) - (cor(d$y,d$x2)*cor(d$x1,d$x2)))/
(1 - cor(d$x1,d$x2)^2)

#How can we interpret b1? It is a standardized coefficient
# for every increase in 1 SD the y will increase by the standardized beta coefficient
#Let's do the same for the second predictor

b2 = (cor(d$y,d$x2) - (cor(d$y,d$x1)*cor(d$x1,d$x2)))/
(1 - cor(d$x1,d$x2)^2)

#Okay let's move onto another example!

#Recall what multicollinearity is...

#If it's there, what are the consequences?

#Let's first create some fake data where the predictors are HIGHLY #correlated
cor.matrix = matrix(c(1, .15, .25,
					 .15,  1,  .9,
					 .25, .9,   1), nrow = 3, ncol = 3, byrow = 				         TRUE)
set.seed(122291)
data = as.data.frame(mvrnorm(200, c(0,0,0), 
	cor.matrix, empirical = TRUE))
	
#Let's first check if the data are highly correlated like we #specified

colnames(data) = c("Y", "X1", "X2") #renaming the columns...
cor(data$X1, data$X2)
	#very high correlation like we specified
	
#Now fit a linear regression model predicting Y using X1 and X2

mod3 = lm(Y ~ X1 + X2, data = data)
summary(mod3)

#First consequence...the predictors are significant

#Another consequence of two highly correlated predictors is that you must take your model results with a grain of salt

#Because two predictors are highly correlated, the individual #results of one predictor may not be valid

#Let's compare to an example where the predictors are not so highly correlated

cor.matrix = matrix(c(1, .15, .25,
					 .15,  1,  .2,
					 .25, .2,   1), nrow = 3, ncol = 3, byrow = 				         TRUE)
set.seed(122291)
data2 = as.data.frame(mvrnorm(200, c(0,0,0), 
	cor.matrix, empirical = TRUE))

colnames(data2) = c("Y", "X1", "X2") #renaming the columns...
cor(data2$X1, data2$X2)
	#very high correlation like we specified
	
#Now fit a linear regression model predicting Y using X1 and X2

mod4 = lm(Y ~ X1 + X2, data = data2)
summary(mod4)

mod3$coef;mod4$coef #What are some significant differences between the coefficients? 

#Empirical example

#We're going to use the example from last lab, it's in the Week 2 folder

example = read.csv("Tworek&Cimpian_2016.csv") 

#feel free to upload using upload data in Rstudio...otherwise use read.csv() or set working directory

#Last lab we fit the models using only one predictor (i.e. simple linear regression), today we'll use multiple predictors in one regression model

mod5 = lm(Should_Score ~ Inherence_Bias + Ought_Score, data = example)

summary(mod5)

#What can we say about the predictors?

#Say your theory said that there should be an interaction between the predictors

interaction = lm(Should_Score ~ Inherence_Bias * Ought_Score, data = example) 
	#simply put * between the predictors to get the main effects 	as well as their interactions
	
summary(interaction)

#Say we wanted to compare the models...

#How can we do this statistically?

#Two ways:
#1. AIC (lower is better)
#2. Likelihood ratio test

#Let's compare our two models: mod5 and interaction

#For AIC...
AIC(mod5);AIC(interaction)
	#what do we conclude?
	
#We can also conduct what is called a likelihood ratio test

anova(mod5,interaction)
	
#Examine p.value. If less than .05, then we favor the 			alternative model
	
#What can we conclude according to the AIC and LRT?

#So it seems that an interaction isn't needed

#Building a model using step-wise regression

#Start backwards...
full.model = lm(Should_Score ~ excluded + RavensProgressiveMatrix_sum + Inherence_Bias + Good_Score + Ought_Score + Belief_in_Just_World, data = example)

backwards.model = step(full.model, direction = c("backward"))

#Do forward
intercept.model = lm(Should_Score ~ 1, data = example)
	#start with intercept only model
	
forwards.model = step(intercept.model, direction = c("forward"), scope = ~excluded + RavensProgressiveMatrix_sum + Inherence_Bias + Good_Score + Ought_Score + Belief_in_Just_World)

#Do the procedures give you the same final model?