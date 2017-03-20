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

#4
d = read.csv("moderationFinalExam.csv")

#4a 
m.int = lm(y  ~ x1 * x2, data = d)
summary(m.int)
c = coef(m.int)
c

#4b  plot moderation of x2 on x1 and y
n2sd <- -1*sd(d$x2)+mean(d$x2)
p2sd <- 1*sd(d$x2)+mean(d$x2)
csd <- mean(d$x2)
n2sd;csd;p2sd

coef(m.int)
d$hn2 <- coef(m.int)[1] + coef(m.int)[3]*n2sd +
  coef(m.int)[2]*d$x1 + coef(m.int)[4]*n2sd*d$x1
d$hcsd <- coef(m.int)[1] + coef(m.int)[3]*csd +
  coef(m.int)[2]*d$x1 + coef(m.int)[4]*csd*d$x1
d$hp2 <- coef(m.int)[1] + coef(m.int)[3]*p2sd +
  coef(m.int)[2]*d$x1 + coef(m.int)[4]*p2sd*d$x1


d <- d[order(d$x1),]

plot(d$x1,d$y,
     type = "n",
     main = "Moderated Associations of x1 and y by x2",
     ylab = "Y",
     xlab = "X1")
lines(d$x1,d$hn2, lwd = 3, col = "blue")
lines(d$x1,d$hcsd, lwd = 3, col = "red")
lines(d$x1,d$hp2, lwd = 3, col = "green")

legend("bottomleft",bty = "n", lty = c(1,1,1),col = c("blue","red","green"),
       legend = c("-1SD","0SD","1SD"))

#4c mean center predictors 
d$x1 = d$x1 - mean(d$x1)
d$x2 = d$x2 - mean(d$x2)

m.int2 = lm(y  ~ x1 * x2, data = d)
summary(m.int2)
c = coef(m.int2)

# plot moderation again after mean centering
n2sd <- -1*sd(d$x2)+mean(d$x2)
p2sd <- 1*sd(d$x2)+mean(d$x2)
csd <- mean(d$x2)
n2sd;csd;p2sd

coef(m.int2)
d$hn2 <- coef(m.int2)[1] + coef(m.int2)[3]*n2sd +
  coef(m.int2)[2]*d$x1 + coef(m.int2)[4]*n2sd*d$x1
d$hcsd <- coef(m.int2)[1] + coef(m.int2)[3]*csd +
  coef(m.int2)[2]*d$x1 + coef(m.int2)[4]*csd*d$x1
d$hp2 <- coef(m.int2)[1] + coef(m.int2)[3]*p2sd +
  coef(m.int2)[2]*d$x1 + coef(m.int2)[4]*p2sd*d$x1


d <- d[order(d$x1),]

plot(d$x1,d$y,
     type = "n",
     main = "Moderated Associations of x1 and y by x2",
     ylab = "Y",
     xlab = "X1")
lines(d$x1,d$hn2, lwd = 3, col = "blue")
lines(d$x1,d$hcsd, lwd = 3, col = "red")
lines(d$x1,d$hp2, lwd = 3, col = "green")

legend("bottomleft",bty = "n", lty = c(1,1,1),col = c("blue","red","green"),
       legend = c("-1SD","0SD","1SD"))

#5a
d <- read.table("~/walter/regression_204b/fat.txt", quote="\"", comment.char="")
colnames(d) =  c("id","age","menarche.a","menarche.t","pfat")
