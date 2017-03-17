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
p <- ggplot(data = dlong, aes(x = PD, y = pdScore, group = id))
p + geom_line()

#spaghetti plot for the social circle distribution of wealth in sample
p <- ggplot(data = dlong, aes(x = SC, y = scScore, group = id))
p + geom_line()

#recode wonky ones
library(car)
d$redist2 = recode(d$redist2, '1=6; 2=5; 3=4; 4=3; 5=2; 6=1')
d$redist4 = recode(d$redist4, '1=6; 2=5; 3=4; 4=3; 5=2; 6=1')

# composite of redistri
d = d %>% mutate(Redist.Comp = alpha(d$redist1,d$redist2,d$redist3,d$redist4))

