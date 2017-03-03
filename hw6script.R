# Walter Reilly
# HW 6 
# 3.2.17

library(afex)
d = read.csv("cinteraction.csv")

plot(d$fluidint,d$relevantknow)



m1 = lm(d$time ~ d$fluidint * d$relevantknow, data =d)
summary(m1)

library(ggplot2)

x1 <- scale(d$relevantknow)
x2 <- scale(d$fluidint)
y <- scale(d$time)
dat <- data.frame(y=y,x1=x1,x2=x2)
res <- lm(y~x1*x2,data=dat)
z1 <- z2 <- seq(-1,1)
newdf <- expand.grid(x1=z1,x2=z2)


p <- ggplot(data=transform(newdf, yp=predict(res, newdf)), 
            aes(y=yp, x=x1, color=factor(x2))) + stat_smooth(method=lm)
p + scale_colour_discrete(name="FluidInt") + 
  labs(x="Relevant Knowledge", y="Time") + 
  ggtitle("Interaction of Relevant Knowledge and Fluid Intelligence")
  scale_x_continuous(breaks=seq(-1,1)) + theme_bw()
