#PSC 204b
#Winter 2017
#Lab04
#Ryan Hutchings & Tim Bahn

#Load libraries ---------------------------------------------------------------------------------------

library(MASS)
library(MBESS)
library(lavaan)

#Simulate data ----------------------------------------------------------------------------------------

#Some data for the next section

c2 <- matrix(c(
    1,   .2,   .2,   .25,  .15,   .1,   .3,   .2,
   .2,    1,   .1,    .1,   .1,   .1,   .1,   .1,
   .2,   .1,    1,    .1,   .1,   .1,   .1,   .1,
  .25,   .1,   .1,     1,   .1,   .1,   .1,   .1,
  .15,   .1,   .1,    .1,    1,   .1,   .1,   .1,
   .1,   .1,   .1,    .1,   .1,    1,   .1,   .1,
   .3,   .1,   .1,    .1,   .1,   .1,    1,   .1,
   .2,   .1,   .1,    .1,   .1,   .1,   .1,    1), 
	nrow = 8, ncol = 8, byrow = TRUE)
set.seed(2615)
d <- as.data.frame(mvrnorm(1000, rep(0,8), c2, empirical = TRUE))
colnames(d) <- c("y","n1","n2","n3","a1","a2","p1","p2")
cor(d)
d$id <- 1:1000

#Hierarchical regression -------------------------------------------------------------------------------

#It is simply the entering of predictors as chunks
#in an iterartive fashion
#Sometimes praised as theoretically guided
#I don't know that I fully agree with this,
#either as a general statement, or as a contrast
#to "data driven" approaches.

#data set d
colnames(d)

m1 <- lm(y ~ n1 + n2 + n3, data = d)
summary(m1)
	#Entered all n-group predictors	
	#sig model and sig predictors

m2 <- lm(y ~ n1 + n2 + n3 +
		 a1 + a2, data = d)
summary(m2)
	#added a-group predictors
	#only a1 is sig, but overal
	#model is sig

anova(m2,m1)
	#difference in R^2 between models is sig.
	#m2 explains sig. more variance in y
	#anova() is used to compare SS explained
	#between nested models based on the
	#difference in df

m3 <- lm(y ~ n1 + n2 + n3 +
		 a1 + a2 +
		 p1 + p2, data = d)
summary(m3)
	#added p-group predictors
	#both p-group predictors sig.
	#as is overall model

anova(m3,m2)
	#m3 predicts sig. more variance
	#in outcome than m2

#In each case, anova() indicated that the next model
#m2 vs. m1, m3 vs. m2, explained significantly more
#variance in the outcome, y, than the previous model
#In each case, we would retain the addition of the
#grouped predictors that were added.

#Moreover, looking at this final model, I would say it
#would not matter what order we entered these groups in.
#Any order of grouped predictor entry would have resulted
#in retention of the grouped predictors in the model.
#This is not always the case.

#For example, if a1 and a2 were not grouped, but instead
#both represented some unique group, then the order that a2
#was entered into the model would determine whether it was
#(initially) retained in the model.

#Hierarchical regressions can have individual items removed,
#but this shows a different decision criteria and is no
#longer consistent with grouped entry retention decisions.


#For example:
set.seed(50)
d$a3 <- d$a2+rnorm(1000,0,.25)
d$a4 <- d$a2+rnorm(1000,0,.25)
d$a5 <- d$a2+rnorm(1000,0,.25)
d$a6 <- d$a2+rnorm(1000,0,.25)
d$a7 <- d$a2+rnorm(1000,0,.25)
d$a8 <- d$a2+rnorm(1000,0,.25)
d$a9 <- d$a2+rnorm(1000,0,.25)
d$a10 <- d$a2+rnorm(1000,0,.25)

m1 <- lm(y ~ n1 + n2 + n3, data = d)
summary(m1)
m2 <- lm(y ~ n1 + n2 + n3 +
		 a1 + a2 + a3 + 
		 a4 + a5 + a6 + 
		 a7 + a8 + a9 + a10, data = d)
summary(m2)
anova(m2,m1)
	#The addition of the a-grouped predictors
	#is no longer sig (though it's close).
	#Too many of the grouped predictors did
	#not contribute sig. variance explained
	#in the outcome. Even though a1 is a sig.
	#predictor, a strictly hierarchical
	#decision to retain the predictor
	#group would indicate we should reject m2
	#and the a-grouped predictors.
	#But this is silly, a1 is a sig. predictor.

######################################################

#Data driven approach?

set.seed(1482)
choose <- sample(1:1000, 500, replace = FALSE)

da <- d[which(d$id %in% choose),]
	#which id values are in the choose vector

db <- d[which(!(d$id %in% choose)),]
	#which id values are not in the choose vector

which(da$id %in% db$id)
	#which da id values are in the db id values? None
	#that means we have successfully split the data
	#file into two sets of 500 observations


ma <- lm(y ~ n1 + n2 + n3 +
		 a1 + a2 +
		 p1 + p2, data = da)

step(ma, direction = "backward")
	#This procedure used AIC, not R^2
	#However, it could be done using
	#R^2

#Now we can fit this model to our other data
#set

mb <- lm(y ~ n1 + n2 + n3 +
		 p1 + p2, data = db)
summary(mb)
	#Model is sig., all predictors included
	#are sig. predictors.
	#What about a1 though?
	#It was sig. in the hierarchical

mb2 <- lm(y ~ n1 + n2 + n3 +
		 a1 + 
		 p1 + p2, data = db)
anova(mb2,mb)
	#Based on change in R^2, a model with a1
	#explains sig. more variance in the outcome
	#than the model without a1
	#for shame step() function, for shame!

summary(mb2)$r.square;summary(mb)$r.square

#Or, we can treat those estimates as constraints

library(lavaan)

mb1 <- '
	y ~ .17*n1 + .119*n2 + .19*n3 + .249*p1 + .146*p2
	y~1
'

fit1 <- sem(mb1, data = db)
summary(fit1, fit.measures = TRUE)

	#The model with constraints fits the data
	#well. Not only do we get roughly the same
	#estimates in a regression model, but in
	#a regression using SEM we can make the
	#constraints on the regression paths
	#to be the same as those found using
	#stepwise regression.
	#All of this falls under cross validation.

#################################################
#################################################

set.seed(123)
x1 <- runif(100,-5,5)
set.seed(321)
y1 <- .3*x1 + rnorm(100,0,1)
set.seed(321)
y2 <- .3*x1 + -.2*x1^2 + rnorm(100,0,1)
set.seed(321)
y3 <- .3*x1 + -.2*x1^2 + .05*x1^3 + rnorm(100,0,1)

#################################################

#Polynomial regression models

m1y1 <- lm(y1 ~ x1)
	#line
m2y1 <- lm(y1 ~ x1 + I(x1*x1))
	#plus quadratic
m3y1 <- lm(y1 ~ x1 + I(x1*x1) + I(x1*x1*x1))
	#plus cubic

#same for y2
m1y2 <- lm(y2 ~ x1)
m2y2 <- lm(y2 ~ x1 + I(x1*x1))
m3y2 <- lm(y2 ~ x1 + I(x1*x1) + I(x1*x1*x1))

#same for y3
m1y3 <- lm(y3 ~ x1)
m2y3 <- lm(y3 ~ x1 + I(x1*x1))
m3y3 <- lm(y3 ~ x1 + I(x1*x1) + I(x1*x1*x1))

#Comparing fit for y1
anova(m3y1,m2y1)
anova(m2y1,m1y1)
	#Apparently no quadratic or cubic effect
	#for y1

#Comparing fit for y2
anova(m3y2,m2y2)
anova(m2y2,m1y2)
	#Sig. improvement in R^2 from linear to
	#quadratic, but not for quadratic to
	#cubic

#Comparing fit for y3
anova(m3y3,m2y3)
anova(m2y3,m1y3)
	#Sig. improvement from linear to quadratic
	#and from quadratic to cubic.

#How well do our best models do?
par(mfrow = c(2,2))
plot(x1,y1)
	points(x1,predict(m1y1),col = "green", cex = 1.5)
plot(x1,y2)
	points(x1,predict(m2y2),col = "green", cex = 1.5)
plot(x1,y3)
	points(x1,predict(m3y3),col = "green", cex = 1.5)

#That's pretty good.

#Would step() select the same models we did?

step(m3y1)

step(m3y2)
	
step(m3y3)

#So, step() agreed this time with what we came up with.
#There's still a long road ahead, but the healing process
#between step() and I has begun.
