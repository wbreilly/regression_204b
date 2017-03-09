# Walter Reilly
# 3.7.17

library(tidyverse)

# setup df
d = read.table("aids.txt", quote="\"", comment.char="")
colnames(d) = c("ID","Treatment","Age","Gender","Week","logCD4")

# get random IDs
rand.samp = sample.int(1313, size = 50, replace = FALSE)
# create new df with rand IDS
dspag = d %>% filter(ID %in% rand.samp)

#spag plot
p <- ggplot(data = dspag, aes(x = Week, y = logCD4, group = ID, colour = factor(Treatment)))
p  + geom_line() + ggtitle("AIDS") + labs(colour = "Treatment")

######################
#2
library(nlme)

# mixed effects mod with random intercept for ID
d$Treatment = as.factor(d$Treatment)
m1 = lme(logCD4 ~ Week + Treatment + Gender , random = ~ 1  | ID, method = "REML", data = d) #random intercept only
summary(m1)

#3
# mixed effects model with random int and random slope for week
m2 = lme(logCD4 ~ Treatment + Gender , random = ~ 1 + Week | ID, method = "REML", data = d) #random intercept only
summary(m2)

#4
#compute AIC BIC and LRT
anova(m1,m2)
