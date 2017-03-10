#extra credit R primer
#Tim Banh & Ryan Hutchings

#load in the extra credit assignment
d = read.csv("Dawtry Sutton and Sibley 2015 Study 1a.csv")

#Reshape data into a long format ===========
names(d)[1] <- "id" #since we don't want the special character

#reshape multiple variables at once
dlong = reshape(d, idvar = "d", varying = c(list(names(d)[2:12]), list(names(d)[15:25])), 
        v.names = c("pdScore", "scScore"), timevar = c("PD", "SC"),
        times = c(15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165), #times has to have the same values for both variables
        direction = "long"
)

#reorder by subject number
dlong = dlong[order(dlong$id),]

#spegetti plot of the perceived distribution of wealth in US
require(ggplot2)
p <- ggplot(data = dlong, aes(x = PD, y = pdScore, group = id))
p + geom_line()

#spegetti plot for the social circle distribution of wealth in sample
p <- ggplot(data = dlong, aes(x = SC, y = scScore, group = id))
p + geom_line()

#computing reliability of composite scores --------------

#we often use scales in psychological research in which items in the scale measure the same 
#underlying construct. In this case, researchers use a variety of techniques to combine items
#and reduce the dimensionality of the data

#in the extra credit data, we have a measure of attitudes toward economic redistribution
#in which several items all theoretically measure the same attitude

#Dawtry et al collapse these four items into a composite scale
#let's see if we can gather some evidence to support this decision

#cronbach's alpha ============

redistrAttitudeData = data.frame(d$redist1, d$redist2, d$redist3, d$redist4)

#let's take a peak at the correlation among the items
cor(redistrAttitudeData)

#it looks like some of the items in this scale are reverse coded
#let's recode the items

#install.packages("car")
library(car)
d$redist2 = recode(d$redist2, '1=6; 2=5; 3=4; 4=3; 5=2; 6=1')
d$redist4 = recode(d$redist4, '1=6; 2=5; 3=4; 4=3; 5=2; 6=1')

redistrAttitudeData = data.frame(d$redist1, d$redist2, d$redist3, d$redist4)

#install.packages("psych")
library(psych)

#compute cronbach's alpha among the items
#alpha = 0.8 is usually good reliability
alpha(redistrAttitudeData)

#interclass correlation ===================

#interclass correlation is another measure of reliability
#a more conservative test of reliability of the measure
ICC(redistrAttitudeData) #average_random_raters

#an interclass correlation that approaches 1 is good! 
