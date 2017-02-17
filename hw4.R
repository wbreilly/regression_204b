# Walt Reilly
# HW 4
# 2.16.17

d = read.csv("~/walter/regression_204b/darktriad.csv")

library(MASS)
library(MBESS)
library(lavaan)
library(tidyverse)

# 1
m1 = lm(y~ n1 + n2,data = d)
summary(m1)

m2 = lm(y~ n1 + n2 + m1 + m2,data = d)
summary(m1)

anova(m1,m2)

m3 = lm(y~ n1 + n2 + m1 + m2 + p1 + p2,data = d)
summary(m3)

anova(m2,m3)

#2
# interaction model 
m4 = lm(y~ m1 * m2 * p1 * p2 * n1 * n2,data = d)
summary(m4)
backwards.model = step(m4, direction = c("backward"))
# AIC gets to -46.084! but tons of ridiculuous interaction terms

#try forward 
intercept.model = lm(y ~ 1, data = d)
#start with intercept only model

forwards.model = step(intercept.model, direction = c("forward"), scope = ~n1 * n2 * m1 * m2 * p1 * p2) 

best.model = lm(y ~ p1 + n1 + m1 + n2 + m2 + n1*m1 + n1*n2, data = d)
summary(best.model)
anova(m3,best.model)

#3
d = read.csv("~/walter/regression_204b/poly.csv")
m1y1 <- lm(y1 ~ x1, data = d)
#line
m2y1 <- lm(y1 ~ x1 + I(x1*x1),data = d)
#plus quadratic
m3y1 <- lm(y1 ~ x1 + I(x1*x1) + I(x1*x1*x1),data = d)
#plus cubic
# summaries
summary(m1y1)
summary(m2y1)
summary(m3y1)

#same for y2
m1y2 <- lm(y2 ~ x1,data = d)
m2y2 <- lm(y2 ~ x1 + I(x1*x1),data = d)
m3y2 <- lm(y2 ~ x1 + I(x1*x1) + I(x1*x1*x1), data = d)
 
#same for y3
m1y3 <- lm(y3 ~ x1,data = d)
m2y3 <- lm(y3 ~ x1 + I(x1*x1),data = d)
m3y3 <- lm(y3 ~ x1 + I(x1*x1) + I(x1*x1*x1),data = d)

#Comparing fit for y1
anova(m3y1,m2y1)
anova(m2y1,m1y1)
# Differences! 
# Cubic is best! 

#Comparing fit for y2
anova(m3y2,m2y2)
anova(m2y2,m1y2)
# Quadratic and cubic differ! 
summary(m3y2)
summary(m2y2)
# Cubic wins again!

#Comparing fit for y3
anova(m3y3,m2y3)
anova(m2y3,m1y3)
# But in the village there was trouble!
summary(m2y3)
summary(m1y3)
# Awkward, quadratic wins... :(


