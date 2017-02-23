# HW5
# Walter Reilly
# Created: 2.23.17

library(tidyverse)

d = read.csv("~/walter/regression_204b/nonlinearhw.csv")

# just leave this here
abline(lm(formula = y1 ~ -exp(x1), data = d), col = "red")

#1a plot it
plot(d$x1,d$y1)
plot(d$x1,d$y2)
plot(d$x1,d$y3)

#1b
m1 = lm(y1 ~ -(x1)^3, data = d)
summary(m1)
c = coef(m1)
curve(c[1] + -(c[2]*x), add = TRUE, lty = 1, col = "red")
 

