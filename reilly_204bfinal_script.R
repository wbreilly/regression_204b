# Walter Reilly
# 3.19.17
# 204b final

#3
library(tidyverse)
# 3 load dat data 
d = read.csv("pigeon.csv")

#3a have a looksies 
plot(d$trial,d$pecks, xlab = "Trial", ylab = "Pecks", main = "Pigeon Pecking")

#3b
m1 = lm(d$pecks ~ d$trial)
summary(m1)


#add line to plot
abline(lm(formula = pecks ~ trial, data = d), col = "blue", lwd = 2)

# use quadratic
m2 = lm(pecks ~ trial + I(trial^5), data = d)
summary(m2)

# add line
c = coef(m2)
 curve(c[1] + c[2]*x + c[3]*(x^3), add = TRUE, lty = 1, col = "green",lwd = 2)


library(nlme)
#sine model
m3 = nls(pecks ~ a + b*(trial) +  sin(trial), start = list(a = 0, b = 0), data = d)
summary(m3)


c = coef(m3)
curve(c[1] + c[2]*x + sin(x), add = TRUE, lty = 1, col = "red", lwd = 2)

#add legend
legend("topleft", bty = "n", legend = c("lm(y~x) ", "lm(y~ x + I(x^5)) ", "nls(y~ a + b *x + sin(x)) "), col = c("blue","green","red"), pch = 15, inset = c(0.05, 0.1))


#compare 
AIC(m1);BIC(m1);
AIC(m2);BIC(m2);
AIC(m3);BIC(m3);
