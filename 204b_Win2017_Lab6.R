#Lab 6
#Ryan Hutchings & Tim Bahn
#Winter 2017

# Plotting regression interactions in R -----------------------------------------------

d <- read.csv("interaction01.csv")

summary(d)

#generate an interaction term for female*x
d$xf <- d$x*d$female

#z-norm all predictors
d$zy <- scale(d$y)[,1]
d$zx <- scale(d$x)[,1]
d$zfemale <- scale(d$female)[,1]

#generate 3-way interaction term
d$zxf <- d$zx*d$zfemale

#Correlations between predictors and their interaction term
#Are the correlations going to change after the z-transformation?

cor(d[,c('x','female','xf')])
cor(d[,c('zx','zfemale','zxf')])
  #linear transformations do not change the relationships b/w the variables

m1 <- lm(y ~ x*female, data = d)
m2 <- lm(zy ~ zx*zfemale, data = d)

summary(m1)
summary(m2)
  #note that the x*female interaction term is the same 
  #regardless of whether the regression is standardized

#####################################################

#Interpretation of the interaction

m1

#on your hw this week, we will ask you to interpret dummy-coded interactions
#what is the value of y when x = 0? What does this mean?
#what is the regression equation for male participants (i.e., female = 0)?
#when x is equal to 0, what the is the expected y-value for males?
#what is the regression equation for female participants? (i.e., female = 1)?
#when x is equal to 0, what is the expected y-value for females?

















#b0: intercept when participants are male
#b1: slope of x & y for males
#b0 + b2: intercept when participants are female
#b2 + b3: slope of x & y for females

#####################################################

#Simple Effects

#split the data by female variable
df <- d[which(d$female == 1),]
dm <- d[which(d$female == 0),]

#separate regression equations for female and male participants
m1f <- lm(y ~ x, data = df)
m1m <- lm(y ~ x, data = dm)

summary(m1f)
summary(m1m)

#plot the differences for females and males 
par(mfrow = c(1,3))
plot(d$x, d$y)

#Pretty evident isn't it?
abline(lm(d$y ~ d$x), col = "red")
plot(dm$x, dm$y)
abline(lm(y ~ x, data = dm), col = "red")
plot(df$x, df$y)
abline(lm(y ~ x, data = df), col = "red")

#or

par(mfrow = c(1,1))
plot(d$x, d$y)
points(df$x, df$y, col = 'red')
points(dm$x, dm$y, col = 'Green')
abline(lm(y~x, data = df), col = 'red')
abline(lm(y~x, data = dm), col = 'green')


#edit path to load data

d <- read.csv("lab06dat.csv")
head(d)
summary(d)

#Dummy coding in regression -----------------------------------------------------------

table(d$serviceanimal)

m1 <- lm(perceivedfree ~ serviceanimal, data = d)
m2 <- lm(independence ~ serviceanimal, data = d)
m3 <- lm(happy ~ serviceanimal, data = d)

anova(m1)
anova(m2)
anova(m3)

summary(m1)
by(d$perceivedfree, d$serviceanimal, mean)-
  by(d$perceivedfree, d$serviceanimal, mean)[[1]]

summary(m2)
by(d$independence, d$serviceanimal, mean)-
  by(d$independence, d$serviceanimal, mean)[[1]]

summary(m3)
by(d$happy, d$serviceanimal, mean)-
  by(d$happy, d$serviceanimal, mean)[[1]]

summary(m3)
t.test(d[d$dog == 1,"happy"],d[d$cat == 1,"happy"],var.equal = TRUE)
t.test(d[d$other == 1,"happy"],d[d$cat == 1,"happy"],var.equal = TRUE)
#differences in SE, t, and p due to differences in df.

#Interactions w/ continuous variables ----------------------------------------------------------

m4 <- lm(happy ~ perceivedfree + independence +
           perceivedfree*independence, data = d)
summary(m4)

#How will you think about the data? Is perceived freedom moderating
#the relation of independence and happiness, or is independence
#moderating the relation of perceived freedom and happiness?

#We'll go with perceived freedom as the moderator.

pf <- seq(min(d$perceivedfree),max(d$perceivedfree),.1)

bi <- coef(m4)[3] + pf*coef(m4)[4]
bi

plot(pf,bi,main = "Moderation of Independence by Perceived Freedom",
     ylab = "Association of Independence and Happiness",
     xlab = "Perceived Freedom Score")
points(
  mean(d$perceivedfree),
  (coef(m4)[3]+coef(m4)[4]*mean(d$perceivedfree)), 
  cex = 3, pch = 16, col = "red")

#or

n2sd <- -1*sd(d$perceivedfree)+mean(d$perceivedfree)
p2sd <- 1*sd(d$perceivedfree)+mean(d$perceivedfree)
csd <- mean(d$perceivedfree)
n2sd;csd;p2sd

coef(m4)
d$hn2 <- coef(m4)[1] + coef(m4)[2]*n2sd +
  coef(m4)[3]*d$independence + coef(m4)[4]*n2sd*d$independence
d$hcsd <- coef(m4)[1] + coef(m4)[2]*csd +
  coef(m4)[3]*d$independence + coef(m4)[4]*csd*d$independence
d$hp2 <- coef(m4)[1] + coef(m4)[2]*p2sd +
  coef(m4)[3]*d$independence + coef(m4)[4]*p2sd*d$independence

d <- d[order(d$independence),]

plot(d$independence,d$happy,
     type = "n",
     main = "Moderated Associations of Independence and Happiness",
     ylab = "Association of Independence and Happiness",
     xlab = "Independence Scores")
lines(d$independence,d$hn2, lwd = 3, col = "blue")
lines(d$independence,d$hcsd, lwd = 3, col = "red")
lines(d$independence,d$hp2, lwd = 3, col = "green")
lines(c(0,0),c(0,10))
legend(-2,38,lty = c(1,1,1),col = c("blue","red","green"),
       legend = c("-1SD","0SD","1SD"))

