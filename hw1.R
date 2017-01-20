# 204b Homework 1
# Walter Reilly
# 1.19.17

library(tidyverse)

d = read_csv("walter/regression_204b/happy.csv")

# summary stats for adjective composites
sumstats = d[,1:4] %>% summarise_each(funs(mean,sd,median, options(digits = 3)))
sumstats = melt(sumstats)
sumstats = transpose(sumstats)

# counts of dichotomous 
counts = d %>% count(lifegreat)
counts[,3:4] = d %>% count(trauma)

# cors
corr.test(d[,1:4], adjust = "none")

# r squared
cors = cor(d[,1:4])
rsquared = cors^2

# package needed for point biserial correlations
library(ltm)

# code as factor
d$lifegreat = as.factor(d$lifegreat)
d$trauma = as.factor(d$trauma)

# tedious af
biserial.cor(d$happy,d$lifegreat)
biserial.cor(d$excited,d$lifegreat)
biserial.cor(d$electric,d$lifegreat)
biserial.cor(d$jubilant,d$lifegreat)
biserial.cor(d$happy,d$trauma)
biserial.cor(d$excited,d$trauma)
biserial.cor(d$electric,d$trauma)
biserial.cor(d$jubilant,d$trauma)


corr.test(d, adjust = "none")

# contingnecy table
ctab = d %>% count(lifegreat,trauma)
ctab = matrix(ctab$n,nrow = 2, ncol = 2, byrow = TRUE)

phi(ctab)
