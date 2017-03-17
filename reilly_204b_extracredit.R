# Walter Reilly
# 3.16.17
#  extrac credit

library(tidyverse)
library(psych)

dwide = read.csv("Dawtry Sutton and Sibley 2015 Study 1a.csv")

#Reshape data into a long format ===========
names(dwide)[1] <- "id" #since we don't want the special character

#reshape multiple variables at once
d = reshape(dwide, idvar = "dwide", varying = c(list(names(dwide)[2:12]), list(names(dwide)[15:25])), 
                v.names = c("pdScore", "scScore"), timevar = c("PD", "SC"),
                times = c(15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165), #times has to have the same values for both variables
                direction = "long"
)

#reorder by subject number
d = d[order(d$id),]

############ include random effect of subject in XC analyses ###############

#spaghetti plot of the perceived distribution of wealth in US
require(ggplot2)
p <- ggplot(data = d, aes(x = PD, y = pdScore, group = id))
p + geom_line()

#spaghetti plot for the social circle distribution of wealth in sample
p <- ggplot(data = d, aes(x = SC, y = scScore, group = id))
p + geom_line()

#recode wonky ones
library(car)
d$redist2 = recode(d$redist2, '1=6; 2=5; 3=4; 4=3; 5=2; 6=1')
d$redist4 = recode(d$redist4, '1=6; 2=5; 3=4; 4=3; 5=2; 6=1')

library(tidyverse)
library(psych)
# composite of redistri
redistrAttitudeData = data.frame(d$redist1, d$redist2, d$redist3, d$redist4)
d = d %>% mutate(Redist.Comp = alpha(d$redist1,d$redist2,d$redist3,d$redist4))




####################################
# question: what makes people think that household incomes are fairly distributed?
m1 = lm(fairness ~ d$Political_Preference, data =d)
summary(m1)
# first conclusion, the less liberal peopple are, the more likely they are to think that the system is fair

# now build a giant model to id other pertinent vars
# start with interaction model
m.int = lm(d$fairness ~ d$Political_Preference * d$satisfaction * d$Household_Income * d$age * d$gender * d$Population_Inequality_Gini_Index *
            d$Population_Mean_Income * d$Social_Circle_Inequality_Gini_Index * d$Social_Circle_Mean_Income * d$pdScore * d$scScore)
summary(m.int)

#backwards model
backwards.int = step(m.int, direction = c("backward"))
summary

# interaction model 
m4 = lm(y~ m1 * m2 * p1 * p2 * n1 * n2,data = d)
summary(m4)
backwards.model = step(m4, direction = c("backward"))
# AIC gets to -46.084! but tons of ridiculuous interaction terms

#try forward 
intercept.model = lm(y ~ 1, data = d)
#start with intercept only model

forwards.model = step(intercept.model, direction = c("forward"), scope = ~n1 * n2 * m1 * m2 * p1 * p2) 

