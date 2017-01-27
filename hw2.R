# Walter Reilly
# 1_26_17
# hw2

# hand calculations

-1.43/(3*sqrt(.663))

#1a
7.2/(4*3)
2.8/8
1.2/6

#1b
.36^2
.35^2
.2^2

#1c
.6*(4/3)
.35*(4/2)

library(tidyverse)
d = read_csv("walter/regression_204b/scatterplot.csv")

# 1. summary stats
digits=3
sumstats = d %>% summarise(SES.mn = mean(SES),PEI.mn = mean(PEI),LGA.mn = mean(LGA),
                           SES.sd = sd(SES), PEI.sd = sd(PEI), LGA.sd = sd(LGA),
                           SES.med = median(SES),PEI.med = median(PEI),LGA.med = median(LGA))

#2
hist(d$SES,25)
hist(d$PEI,25)
hist(d$LGA,25)

#3
plot(d$SES,d$LGA, main = "LGA x SES")
abline(lm(d$LGA ~ d$SES), col="red")

plot(d$PEI,d$LGA, main = "LGA x PEI")
abline(lm(d$LGA ~ d$PEI), col="red")

plot(d$SES,d$PEI, main = "SES x PEI")
abline(lm(d$PEI ~ d$SES), col="red")

#4
m1 = lm(d$PEI ~ d$LGA )
plot(d$SES,residuals(m1))

m2 = lm(d$SES ~ d$LGA)
plot(d$PEI,residuals(m2))
