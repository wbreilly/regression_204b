# HW5
# Walter Reilly
# Created: 2.23.17

library(tidyverse)

d = read.csv("~/walter/regression_204b/nonlinearhw.csv")

# just leave this here
abline(lm(formula = y1 ~ x1, data = d), col = "red")

#1a plot it
plot(d$x1,d$y1)
plot(d$x1,d$y2)
plot(d$x1,d$y3)

#1b
#standard lm
m1 = lm(y1 ~ x1, data = d)
summary(m1)
abline(lm(formula = y1 ~ x1, data = d), col = "red")

# use quardaratic
nlm1 = lm(y1 ~ x1 + I(x1^2), data = d)
summary(nlm1)
c = coef(nlm1)
curve(c[1] + c[2]*x + c[3]*(x^2), add = TRUE, lty = 1, col = "red")

# try log
nlm2 = nls(y1 ~ a - log(x1), start = list(a = 4), data = d) #think of a as your "intercept" and b as your "slope"
summary(nlm2)
c = coef(nlm2)
curve(c[1] - log(x), add = TRUE, lty = 1, col = "red")

AIC(m1);AIC(nlm1);AIC(nlm2)
BIC(m1);BIC(nlm1);BIC(nlm2)
###########
# predict y2 with x1
#standard lm
m2 = lm(y2 ~ x1, data = d)
summary(m2)
abline(lm(formula = y2 ~ x1, data = d), col = "red")

#sine model
nlm3 = nls(y2 ~ a + sin(x1), start = list(a = 5), data = d)
summary(nlm3)
c = coef(nlm3)
curve(c[1] + sin(x), add = TRUE, lty = 1, col = "red")

# use cubic
nlm4 = lm(y1 ~ x1 + I(x1^3), data = d)
summary(nlm4)
c = coef(nlm4)
curve(c[1] + c[2]*x + c[3]*(x^3), add = TRUE, lty = 1, col = "red")

AIC(m2); AIC(nlm3); AIC(nlm4)
BIC(m2); BIC(nlm3); BIC(nlm4)
###########
# predict y3 with x1
#standard lm
m3 = lm(y3 ~ x1, data = d)
summary(m3)
abline(lm(formula = y3 ~ x1, data = d), col = "red")

# use quadratic
nlm5 = lm(y3 ~ x1 + I(x1^2), data = d)
summary(nlm5)
c = coef(nlm5)
curve(c[1] + c[2]*x + c[3]*(x^2), add = TRUE, lty = 1, col = "red")

# use cubic
nlm6 = lm(y3 ~ x1 + I(x1^3), data = d)
summary(nlm6)
c = coef(nlm6)
curve(c[1] + c[2]*x + c[3]*(x^3), add = TRUE, lty = 1, col = "red")

# try log
nlm7 = nls(y3 ~ a + logb(x1, b= 1.25 ), start = list(a = 0), data = d) #think of a as your "intercept" and b as your "slope"
summary(nlm7)
c = coef(nlm7)
curve(c[1] + logb(x, b = 1.25), add = TRUE, lty = 1, col = "red")

AIC(m3); AIC(nlm5); AIC(nlm6); AIC(nlm7)
BIC(m3); BIC(nlm5); BIC(nlm6); BIC(nlm7)
