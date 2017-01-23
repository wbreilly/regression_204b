#Tim Banh & Ryan Hutchings
#PSC 204b
#Winter 2017
#Lab02

# Load required packages -----------------------------------------------------------------------------------
library(ltm)
library(MASS)
library(MBESS)

# Simple Linear Regression ---------------------------------------------------------------------------

#Last week we examined the relationship between variables.
#This week we will examine whether we can PREDICT the outcome variable
#from a predictor

# Loading in example data ==============================================================================

#There are multiple ways to load in data (or things in spreadsheets) into R-studio

write.csv(file.choose())
  #this will open file explorer or finder and let you browse and find the data to import

#The "better way" to read in data by changing working directory

# getwd()
  #prints your current directory

# setwd()
  #change your working directory to a folder by writing a file path
  #hint: you can use the files tab on the right side of your screen and press the [...]
  #in the righthand upper corner to browse your computer to find the file or folder
  #where you would like to set your directory. Then click the "more" pulldown menu and 
  #click "set on working directory"

path = ("~/Davis/regressionWin17/joesrcode")

setwd(path)

#We are going to load in REAL data from a study by Tworek & Cimpian published in
#Psychological Science in 2016 (doi: 10.1177/0956797616650875)

d = read.csv("Tworek&Cimpian_2016.csv")

# Simple Linear Regression ===================================================================================

#Research Question: Tworek & Cimpian suggest that to the extent that people
#endorse inherent explanations (i.e., roses are given on Valentine's
#because roses are beautiful) people also affirm
#the status quo ("keep things the way they are")

#Belief in a just world is a variable that tells us the extent that
#people believe that life is fair, people get what they deserve from
#world etc etc

#Lets first examine the correlation between these two scores
cor(d$Inherence_Bias, d$Should_Score)
cor.test(d$Inherence_Bias, d$Should_Score)

#Does Inherence_bias predict Should_Score?
mod1 = lm(Should_Score ~ Inherence_Bias, data = d)

summary(mod1)
  #Note the estimate, SE, R^2, and overall test of sig.

#Note that R^2 is the same as squaring the correlation coefficient
cor(d$Inherence_Bias, d$Should_Score)
cor(d$Inherence_Bias, d$Should_Score)^2

#Clean out the clutter 
coef(summary(mod1))

#These are the unstandardized coefficients

#We can get the standardized coefficients
#in two different ways

#1: use the lm.beta function in the QuantPsyc package

install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(mod1)
  #This is the unstandardized regression coefficient
cor(d$Inherence_Bias, d$Should_Score)
  #and it is the same as the correlation coefficient

#2: standardize variables in the model
mod1z <- lm(scale(d$Should_Score)[,1] ~ scale(d$Inherence_Bias)[,1])
mod1z

summary(mod1z)
  #which is also the same as
cor(d$Inherence_Bias, d$Should_Score)

# Centering ============================================================================================

shouldVec <- d$Should_Score
inhVec <- d$Inherence_Bias

#This is the intercept using the data as is

coef(summary(lm(shouldVec ~ inhVec)))
  #or, since we just want to see the estimate

lm(shouldVec ~ inhVec)

#Centering predictor ===================================================================================

#Centering is really adjusting the mean
#of the data. Often this involves centering
#using the mean of the data, making the new
#mean ~ 0
inhVec2 <- inhVec - mean(inhVec)

#This is the intercept with inhVec centered at zero
summary(lm(shouldVec ~ inhVec2))$coef

#$coef allows you to call the coefficient info
#from the lm() summary.

#While the effect of inhVec does not change
#the intercept value does

#Centering outcome  ===============================================================================
shouldVec2 <- shouldVec - mean(shouldVec)

#This is the intercept with should_score centered at zero
summary(lm(shouldVec2 ~ inhVec))

# Centering predictor and outcome =======================================================================

#Finally the intercept with both inhVect and shouldVec
#centered at zero 
lm(shouldVec2 ~ inhVec2)

# Comparing our results ===============================================================================
#or, to compare the intercepts at once
#round() will round numeric values in a vector	
#to a specified decimal place.

ival <- as.data.frame(matrix(round(c(
  coef(lm(shouldVec ~ inhVec))[[1]],
  coef(lm(shouldVec ~ inhVec2))[[1]],
  coef(lm(shouldVec2 ~ inhVec))[[1]],
  coef(lm(shouldVec2 ~ inhVec2))[[1]]
), 3), nrow = 1, ncol = 4))
colnames(ival) <- c('nocenter','inhVec_c','shouldVec_c','both_c')
ival

# Plotting --------------------------------------------------------------------------------------------

#Based on Chapter 4.2 in book 

#par(mfrow()) lets you place multiple graphs on the same plot

#Histograms ============================================================================================
par(mfrow = c(1,2))
hist(d$Inherence_Bias)
hist(d$Belief_in_Just_World)

#Histograms with specified, but varying breaks
par(mfrow = c(1,2))
hist(d$Inherence_Bias, breaks = 10)
hist(d$Inherence_Bias, breaks = 100)

#Density curve
par(mfrow = c(1,2))
plot(density(d$Inherence_Bias))
plot(density(d$Belief_in_Just_World))

#Overlay a normal distribution on your density curve

hist(d$Inherence_Bias, breaks=20, prob=TRUE, 
     xlab="x-variable",
     main="normal curve over histogram")
curve(dnorm(x,
            mean = mean(d$Inherence_Bias),
            sd = sd(d$Inherence_Bias)), 
      col="blue", lwd = 2, lty = 1, 
      add=TRUE, yaxt="n")

#Note that we are "working" on the same plot object
#We could continue to add more and more lines and data to this same object

#Boxplot ==============================================================================================

par(mfrow = c(1,2))
boxplot(d$Inherence_Bias, ylim = c(0,10))
boxplot(d$Belief_in_Just_World, ylim = c(0,10))

#Scatterplot ===========================================================================================
par(mfrow = c(1,1))
plot(d$Inherence_Bias, d$Should_Score)

#Let's make a nice graph
plot(d$Inherence_Bias, d$Should_Score, 
     ylim = c(1,9), # Adding the minimum and maximum Y values
     xlab = "Inherence Bias", # X-axis title
     ylab = "Maintain Status Quo", # Y-axis title
     main = "Motivation to Maintain Status Quo as Function of Inherence Bias") # main title

#Does inherence bias predict maintaining the status quo?
abline(lm(Should_Score ~ Inherence_Bias, data = d), lwd = 5, col = "red")

#Scatterplot Comparisons ===============================================================================

#Visualize two different relationships at once by

windows()
plot(d$Should_Score, d$Inherence_Bias, xlim = c(1,9), ylim = c(1,9))
points(d$Should_Score, d$Belief_in_Just_World, col = "red")

#Windows() opens a new window for a plot
#to be generated in. This is the command
#for a windows platform.
#X11() Unix 
#quartz() Mac 

# Plotting residuals ----------------------------------------------------------------------------------

#Chapter 4.2:3

#Plotting residuals will help us identify whether or not we have specified the model correctly
#If there is systematic error in the residuals, then we are likely using the wrong model

set.seed(32)
x1 <- sample(seq(0,20,.5),200, replace = TRUE)
y1 <- 5 + 2*x1 + .2*x1^2 + -0.015*x1^3 + rnorm(200,0,5)

#Plotting residuals for inspection

par(mfrow = c(1,2))
plot(x1,
     resid(lm(y1 ~ x1 + I(x1^2) + I(x1^3)))
)
plot(x1,
     resid(lm(y1 ~ x1 + I(x1^2)))
)

#Linear model in the residuals?
par(mfrow = c(1,2))

#A cubic model
plot(x1,resid(lm(y1 ~ x1 + I(x1^2) + I(x1^3))))
lines(lowess(x1, 
             resid(lm(y1 ~ x1 + I(x1^2) + I(x1^3))), 
             f = .4), lwd = 3, col = "green")

#A quadratic model
plot(x1,resid(lm(y1 ~ x1 + I(x1^2))))
lines(lowess(x1, 
             resid(lm(y1 ~ x1 + I(x1^2))), 
             f = .4), lwd = 3, col = "green")

#The residual is just the difference from the predicted score
#Regression assumes that the residuals are independent random noise
#When residuals are not randomly distributed, HOUSTON WE'VE GOT A PROBLEM 
#... with how u specified ur model

# Code appendix A -------------------------------------------------------------------------------------

#Lowess smoothed lines [Chapter 4.5]

#Lowess smoothed lines are locally-weighted polynomial regressions
#Lowess smoothed lines are helpful for identifying nonlinear relationships

#Using the x1 and y1 data we generated previously ...

par(mfrow = c(2,3))
plot(x1,y1)
lines(lowess(x1, y1, f = .01), 
      lwd = 3, col = "red")
plot(x1,y1)
lines(lowess(x1, y1, f = .1), 
      lwd = 3, col = "red")
plot(x1,y1)
lines(lowess(x1, y1, f = .3), 
      lwd = 3, col = "red")
plot(x1,y1)
lines(lowess(x1, y1, f = .5), 
      lwd = 3, col = "red")
plot(x1,y1)
lines(lowess(x1, y1, f = .7), 
      lwd = 3, col = "red")
plot(x1,y1)
lines(lowess(x1, y1, f = .9), 
      lwd = 3, col = "red")

#by adjusting the f value in the lowess()
#function we adjust the amount of 
#data used to generate values

#contrast abline() with lowess
plot(x1,y1)
abline(lm(y1~x1), lwd = 3, col = "red")
lines(lowess(x1, y1, f = .5), lwd = 3, col = "green")

#Remember, we can also use predicted values to create lines
summary(lm(y1 ~ x1 + I(x1^2) + I(x1^3)))

#Model with effect of x1, quadratic effect x1, and
#cubic effect x1
plot(x1,y1)
points(x1,fitted(lm(y1 ~ x1 + I(x1^2) + I(x1^3))),
       pch = 16, cex = 1.5, col = "red")
lines(lowess(x1, y1, f = .35), lwd = 3, col = "green")

#A well fitting model can outperform lowess

plot(x1,y1)
points(x1,fitted(lm(y1 ~ x1 + I(x1^2))),
       pch = 16, cex = 1.5, col = "red")
lines(lowess(x1, y1, f = .35), lwd = 3, col = "green")

#But a poorer model generally won't after you adjust
#the lowess function







