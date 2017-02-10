# hw 3
# Walter Reilly
# 2.9.17

library(tidyverse)

y    = c(0, -1, 3, -1, 0, 1)
x1  = c(3, -1, 1, -1, -2, -1)
x2  = c(1, -2, 2, 1, -1, 0)

d = data.frame(y,x1,x2)

# 1a

sumstats = d %>% summarise(y.mean = mean(y), y.sd = sd(y),x1.mean = mean(x1),
                x1.sd = sd(x1),x2.mean = mean(x2), x2.sd = sd(x2))

#1b
cov.mtx = cov(d)
#1c
cor.mtx = cor(d)

#2a
lm(y ~ x1, data = d)

#2b
zd = as.data.frame(scale(d))
lm(y ~ x1, data = zd)

#2c
lm(y ~ x2, data = d)

#2d
lm(y ~ x2, data = zd)

#3a
lm(y ~ x1 + x2, data = d)

#3b
lm(y ~ x1 + x2, data = zd)

#4a
d2 = read.csv("walter/regression_204b/Tworek&Cimpian_2016.csv")

m1 = lm(d2$Should_Score ~ d2$Belief_in_Just_World)
summary(m1)

#4b
m2 = lm(d2$Should_Score ~ d2$Belief_in_Just_World + d2$Good_Score + d2$excluded)
summary(m2)

#4c
AIC(m1);AIC(m2)
anova(m1,m2)

#4d
m3 = lm(d2$Should_Score ~ d2$Belief_in_Just_World * d2$Good_Score * d2$excluded)
summary(m3)
AIC(m2);AIC(m3)
anova(m2,m3)

#4e
m4 = lm(d2$Should_Score ~ d2$Belief_in_Just_World + d2$Good_Score + d2$excluded + d2$RavensProgressiveMatrix_sum
        + d2$Inherence_Bias + d2$Ought_Score)
summary(m4)
backwards.m4 = step(m4, direction = c("backward"))
#forward model

intercept.m4 = lm(Should_Score ~ 1, data = d2)
forward.m4 = step(intercept.m4, direction = c("forward"), scope = ~d2$Belief_in_Just_World + d2$Good_Score + d2$excluded + d2$RavensProgressiveMatrix_sum
                  + d2$Inherence_Bias + d2$Ought_Score)



