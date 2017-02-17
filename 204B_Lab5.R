#Tim Banh & Ryan Hutchings
#PSC 204B
#Winter 2017
#Lab 5

##################################
###Nonlinear Regresssion Models###
##################################

#We're going to be working with nonlinear data today, which is fun
#because that's what Tim researches (and possibly Ryan)!

#Nonlinear data is exactly what it sounds like: data that takes on 
#a trajectory that doesn't follow a straight line

#What is different: change isn't the same for each increase in x
#(change isn't constant across time)

#Word of warning for you SPSS users, I don't know if SPSS supports
#nonlinear modeling. I would caution that R or SAS is the best
#stats software for this upcoming homework

#That being said, I (Tim) don't really know how to use SPSS so I
#can't help you if you wanted to use SPSS

#Let's first look at nonlinear model examples

set.seed(300)
temp <- runif(300,0,10)

#Some examples of nonlinear functions
par(mfrow = c(3,3))
plot(temp, exp(temp),main = "Exponential")
plot(temp, -exp(temp),main = "Neg. Exponential")
plot(temp, log(temp),main = "Natural Log")
plot(temp, -log(temp),main = "Neg. Natural Log")
plot(temp, sin(temp),main = "Sin")
plot(temp, cos(temp),main = "Cos")

#Load the nonlinear.csv data set into R

data = read.csv('~/walter/regression_204b/nonlinear.csv')

#First examine some plots
plot(data$x1,data$y1) 
	#looks curvilinear to me (more specifically an exponential 		curve)

plot(data$x1,data$y2)
	#looks curvilinear again (negative logarithmic curve)
	
plot(data$x1,data$y3)
	#most curvilinear thing I've ever seen! (sine curve)

#Fit a model predicting y1 using x1
mod1 = lm(data$y1 ~ data$x1)
plot(data$x1,data$y1);abline(lm(formula = data$y1 ~ data$x1), col = "red")
	#seems to fit well except for lower/higher values of x1

#Fit a model predicting y2 using x1
mod2 = lm(data$y2 ~ data$x1)
plot(data$x1,data$y2);abline(lm(formula = data$y2 ~ data$x1), col = "red")
	#again, the model seems to fit well but for the tails of the 	data, the model is not fitting as much

#Fit a model predicting y3 using x1
mod3 = lm(data$y3 ~ data$x1)
plot(data$x1,data$y3);abline(lm(formula = data$y3 ~ data$x1), col = "red")
	#this model fits terribly as you can see from the plot. Any 	ideas why?
	
#General take-home message: it's not always tenable to assume that a line will fit your data and that change is constant across time

#Is there some way to do nonlinear modeling in R? 
#Of course! Instead of lm(), you would use nls()

#Nonlinear models are a little more complicated

#Heuristically, it makes sense, right? A curve is more complicated 
#than a straight line so estimating a nonlinear curve is
#considerably more complicated than a line

#For nonlinear models, there are not closed form solutions to the 
#log-likelihood so we need to APPROXIMATE solutions

#Thus, we need to give R starting values to search for the optimal
#solution and get parameter estimates

#Bad starting values = model will not converge

#Fit a model predicting y1 using x1 with an exponential function
nlmod1 = nls(y1 ~ a + exp(b*x1), start = list(a = 0, b = 1), data = data) #think of a as your "intercept" and b as your "slope"
summary(nlmod1)

#Examine the model overlaid on a plot
plot(data$x1,data$y1)
c = coef(nlmod1)
curve(c[1] + exp(c[2]*x), add = TRUE, lty = 1, col = "red")
	#the curve looks like a better fit than a line, right?

#Compare model fit of the linear to the nonlinear model
AIC(mod1);AIC(nlmod1) #AIC favors the nonlinear model, obviously
BIC(mod1);BIC(nlmod1) #BIC also favors the nonlinear model

#Don't always have to use nls() to construct a nonlinear model.
#You can use polynomial models to get a nonlinear shape

#Example: y = b0 + b1*x + b2*x^2

plot(data$x1,data$y2)
	#looks like an upside down parabola??

#Fit a nonlinear model predicting y2 using x1 and a quadratic     #function
nlmod2 = lm(y2 ~ x1 + I(x1^2), data = data)
summary(nlmod2)
c = coef(nlmod2)
curve(c[1] + c[2]*x + c[3]*(x^2), add = TRUE, lty = 1, col = "red")
	#again, the curve looks like a good fit to the data!

#Compare model fit of the linear to the nonlinear model
AIC(mod2);AIC(nlmod2) #fit of the nonlinear model is better,
#although the difference is not as drastic as the previous example
BIC(mod2);BIC(nlmod2) #BIC favors the nonlinear model, but again,
#not by much

plot(data$x1,data$y3)
	#looks like a sine wave

#Fit a model predicting y3 using x1 with a sine curve
nlmod3 = nls(y3 ~ a + sin(x1), start = list(a = 15), data = data)
summary(nlmod3)
c = coef(nlmod3)
curve(c[1] + sin(x), add = TRUE, lty = 1, col = "red")
	#looking at the line on the plot, this model actually looks 	really...bad
	
#Try another model...let's give a "slope" to the sine function
nlmod4 = nls(y3 ~ a + b*sin(x1), start = list(a = 1, b = 10), data = data)
summary(nlmod4)
c = coef(nlmod4)

#Examine the curve overlaid on the plot
plot(data$x1,data$y3)
curve(c[1] + c[2]*sin(x), add = TRUE, lty = 1, col = "red")
	#much better!

#Compare model fit
AIC(mod3);AIC(nlmod3) #again, the nonlinear model is better, but not by much...
BIC(mod3);BIC(nlmod3)
AIC(nlmod3);AIC(nlmod4) #big improvement in model fit for the better sine wave model
BIC(nlmod3);BIC(nlmod4)

##################################################################

#That's it for that example...let's look at another data set

##################################
###Transformations of Variables###
##################################

#Reading in another nonlinear data set
data2 = read.csv('~/walter/regression_204b/nonlinear2.csv')

#Examine a preliminary plot of the variables
plot(data2$x1,data2$y1)

#Looks like a negative exponential curve

#This data set looks like something more applicable to
#psychology...perhaps response time? Takes a long time to respond
#something at first but then they learn and start responding
#quicker

#Fit an exponential model
nlmod5 = nls(y1 ~ a + b*exp(x1), start = list(a = 0, b = 1), data = data2)
summary(nlmod5)
c = coef(nlmod5)

#Examine the curve overlaid on the plot
plot(data2$x1,data2$y1)
curve(c[1] + c[2]*x, add = TRUE, lty = 1, col = "red")
	#doesn't look like a good fit to the data

#This is a problem with nonlinear models, there are multiple
#shapes for your model to take, not just a straight line so you
#have to "guess" what shape is best...

#If I had to guess, I would probably guess that an upside-down
#parabola is best, like in the previous example, so let's fit
#another upside down parabola and see how that fits

#Fit a quadratic model predicting y1 and x1, x1^2
quadratic.mod = lm(y1 ~ x1 + I(x1^2), data = data2)

#Examine summary of the model
summary(quadratic.mod)

#Look at the plot with the model curve overlaid
plot(data2$x1,data2$y1)
c = coef(quadratic.mod)
curve(c[1] + c[2]*x + c[3]*x^2, add = TRUE, lty = 1, col = "red")
	#That fits well and all but what this model might be difficult 	to interpret
	#Linear models are easy to interpret because there are only 	two parameters to interpret

#What if we wanted to transform the one or more variables so that
#we can get a linear trajectory?

head(data2) #the y1 values look like exponentiated powers of
#x1...so what do you think is a good transformation for y1??

#let's take the log of the y1 values
logy1 = log(data2$y1)

#attach the vector to the data set
data2 = cbind(data2,logy1)

#Can we now make a linear model predicting logy1 using x1??
logmod = lm(logy1 ~ x1, data = data2)
summary(logmod)

#Let's look at how this model fits graphically
plot(data2$x1,data2$logy1)
c = coef(logmod)
curve(c[1] + c[2]*x, add = TRUE, lty = 1, col = "red")
	#fits better than the one previous, but I think we could do 	better

#Let's try another transformation...square root of y1?
sqrt.y1 = sqrt(data2$y1)
sqrt.x1 = sqrt(data2$x1)
data2 = cbind(data2,sqrt.x1,sqrt.y1)

sqrt.mod = lm(sqrt.y1 ~ sqrt.x1, data = data2)
summary(sqrt.mod)

plot(data2$sqrt.x1,data2$sqrt.y1)
c = coef(sqrt.mod)
curve(c[1] + c[2]*x, add = TRUE, lty = 1, col = "red")
	#again, this doesn't look perfect

#Sometimes it is difficult to transform your variables to attain a 
#truly linear trajectory

#Take-home message about nonlinear modeling: it's not as simple!

#Need to take into account what type of model you're fitting as
#well as making sure your model is still easy to interpret!