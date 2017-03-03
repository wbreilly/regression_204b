#Tim Banh & Ryan Hutchings
#PSC 204B
#Winter 2017
#Lab 7

##################################################################Introduction to Mixed-Effects Models/Random coefficient models/multilevel models/hierarchical linear models
##################################################################

#So far we have assumed that all our measurements have been 
#independent of each other

#For example, in predicting weight from height, each individual is
#not related to another individual

#In class, we introduced the idea of blocking

#Historically the idea of blocking comes from farming

#You wanted to examine crops in blocks but one crop from one patch
#of land is "related" to a crop in the same block

#In the same vein, you want to capture some of that dependence in
#each block

#That is, how much variance in crop yield is related to membership
#to a certain block

#Let's first look at an example of why blocking of individuals
#violates the independence assumption using some simulated data

##################################################################
#########################Motivating Example#######################
##################################################################

#First set seed for reproducible results 
set.seed(1234)

#Population intercept
b0 = 130

#Population slope
b1 = 2.5

#100 individuals
n = 100

#Independent variable
x = rnorm(n, 10, 0.785)

#Standard normal error term
error = rnorm(100)

#Simple linear regression model
y = b0 + b1*x + error

#Plot of the data
plot(x, y, xlab = "Books Read", ylab = "GRE Verbal Score", main = "Scatterplot of Books Read vs. GRE Verbal Score")
mod1 = lm(y ~ x) #fit a model to the simulated data
abline(mod1, col = "red") #overlay the regression line

#This is a simple example of what we've been doing: regressing GRE
#verbal score on number of books read

#We can see a linear trend: as number of books read increases, so
#does GRE verbal score

#Imagine that every "data point" in the plot is a student from one
#university

#Let's do the same thing as above but with multiple universities

#Population parameters for University 1
b0_u1 = 130 #intercept
b1_u1 = 2.5 #slope
n = 100
x = rnorm(n, 10, 0.785) #number of books read as a normal random 					    variable
error = rnorm(100) #standard normal error term
y_u1 = b0_u1 + b1_u1*x + error #population generating model for 							   University 1

#Population parameters for University 2
b0_u2 = 140 #intercept, higher intercept than University 1...
b1_u2 = 1 #slope, ...but smaller slope than University 1
y_u2 = b0_u2 + b1_u2*x + error #population generating model for 							   University 2

#Population parameters for University 3
b0_u3 = 120 #intercept
b1_u3 = 3 #slope
y_u3 = b0_u3 + b1_u3*x + error
	#population generating model for University 3

#Create a combined data frame for all 3 universities
data = data.frame(x, y_u1, y_u2, y_u3)

#Create a linear regression model for each university

mod1 = lm(y_u1 ~ x, data = data) #University 1
mod2 = lm(y_u2 ~ x, data = data) #University 2
mod3 = lm(y_u3 ~ x, data = data) #University 3

#Now we can plot each university on the same plot
plot(x, y_u1, xlab = "Books Read", ylab = "GRE Verbal Score", main = "GRE Regressed on Books Read", col = "red", ylim = c(140, 160))
par(new = TRUE)

plot(x, y_u2, xlab = "Books Read", ylab = "GRE Verbal Score", main = "GRE Regressed on Books Read", col = "blue", ylim = c(140, 160))
par(new = TRUE)

plot(x, y_u3, xlab = "Books Read", ylab = "GRE Verbal Score", main = "GRE Regressed on Books Read", col = "dark green", ylim = c(140, 160))

legend(10.5, 145, c("University 1", "University 2", "University 3"), col = c("red", "blue", "dark green"), pch = c(16, 16))
	#can see that red represents the trajectory of University 1, 	blue = University 2, green = University 3

#Construct a model assuming that everybody is in the same
#"block" (ignore that people are naturally grouped)
combined.data = data.frame(rep(x,3), c(y_u1, y_u2, y_u3))
	#creating data where everybody is lumped into one block
	
names(combined.data) = c("Books", "GRE")
	#renaming the variables
	
mod4 = lm(GRE ~ Books, data = combined.data)

abline(mod4, col = "black") 
	#overall regression line assuming everybody is in the same 		school
	#is this a reasonable assumption??

abline(mod1, col = "red"); abline(mod2, col = "blue"); abline(mod3, col = "dark green")
	#fitting a regression line to each individual university
	
#See that when we ignore blocking, we will get inaccurate results (see plot!)

#Blocks come in all sorts of studies
	#Examples: schools, districts, plots, families, and especially 	subjects
	
	#In context of longitudinal studies, repeated measures are 		gathered on subjects. Treat subjects as "blocks" with repeated 	measures "nested" within individuals
	
	#Recall GRE example, if we treat every repeated measure as 		independent, then we will get inaccurate results
	
	#Problem is, repeated measurements within a subject are 			dependent

##################################################################						#Linear Mixed Models#
##################################################################

#Load in dental data

dental = read.table("/Users/Tim/Desktop/UCDavis/Year_3/PSC_204B/dental.txt", header = FALSE)

names(dental) = c("ID", "Gender", "Time1", "Time2", "Time3", "Time4") #creating variable labels

#Repeated measurements on dental growth measurements on 27 children

summary(dental) #can see measurements are increasing over time

#Data are currently in WIDE format, meaning that every person has one record and each time point occupies one column

#To run a linear mixed model in R, data need to be in LONG format, where each person's time point occupies one line

#Converting dental to long format
long.dental = reshape(dental, idvar = "ID", varying = list(3:6), v.names = "Response", timevar = "Time", direction = "long")

long.dental = long.dental[order(long.dental$ID),] #sorting by ID

#Let's examine a spaghetti plot
install.packages("ggplot2")
require(ggplot2)

p <- ggplot(data = long.dental, aes(x = Time, y = Response, group = ID))
p + geom_line() #can see considerable variability in both response at Time 1 AND in slopes

#Construct a simple linear regression model where Response
#is regressed on Time (this is wrong but I'm doing it to prove a
#point)

dental.mod1 = lm(Response ~ Time + Gender, data = long.dental)
summary(dental.mod1) #coefficients are significant

#We know that each of the 4 repeated measures nested within person
#are correlated with each other though
	#Some people have higher intercepts (at Time 1 have bigger 		jaws), some have lower intercepts
	#Some people have higher slopes (jaw gets bigger as time 		increases), some have lower slopes

#Problem with fitting a simple linear regression is that we assume everybody has a fixed intercept and slope

#Solution: fit a linear mixed-effects model

library(nlme)

dental.mod2 = lme(Response ~ Time + Gender, random = ~ 1 | ID, method = "REML", data = long.dental) #random intercept only

summary(dental.mod2) #lots of new information, I'll explain each 					 one...
	
	#we summarize variance in the intercept by a random intercept 	variance (if it's big, then there's a lot of variability in 	jaw size at Time 1)

#Was including a random effect better than just a simple linear regression?
AIC(dental.mod1);AIC(dental.mod2) #definitely

#What if we hypothesize that there is variability in jaw growth?
	#Solution: include random slope

dental.mod3 = lme(Response ~ Time + Gender, random = ~ 1 + Time | ID, method = "REML", data = long.dental) #random intercept/slope

summary(dental.mod3) #more information than random intercept only model

#Is including a random slope better than just a random intercept?

AIC(dental.mod2);AIC(dental.mod3)
anova(dental.mod2,dental.mod3)	
	#doesn't look like it, but substantive theory might dictate 	that people should have variability in their slopes