#Tim Banh & Ryan Hutchings
#PSC 204B
#Winter 2017
#Lab 8

############################################################################################Logistic Regression######################
###################################################################

#Purpose: Used to predict probabilities, which are constrained to be between 0 and 1

#Normal linear regression model assumes that our response is CONTINUOUS

#Not tenable with probabilities

#Need to establish a link function that transforms a continuous response into a response from 0 to 1

#First load in the data set

donner = read.table("https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson07/donner.txt", header = FALSE)

#The Donner party were a family that left Springfield, Illinois for California (who wouldn't?). They became stranded in the Sierra Nevada Mountains and got stuck in a snowstorm. Out of 87 people, 40 died

names(donner) = c("Age", "Sex", "Survival") #Sex (0 = female, 1 = male), Survival (0 = died, 1 = survived)

#Make sure that Sex and Survival are treated as factors
class(donner$Sex);class(donner$Survival) #they aren't

donner$Sex = as.factor(donner$Sex);donner$Survival = as.factor(donner$Survival)

#Let's examine some summary statistics
summary(donner) #notice we only have 45 subjects - this is a subset of the 87
#20/45 survived

#First construct a normal linear regression model

mod1 = lm(Survival ~ Age + Sex, data = donner) #trick question, lm won't let us do it

#Must use glm() instead
mod2 = glm(Survival ~ Age, family = "binomial", data = donner)
summary(mod2)

#How to interpret a logistic regression model?
coef(mod2) #can't just say the intercept is the probability of survival when every other coefficient is 0. Why?

#In logistic regression, the "y" is actually a logit, which is defined as log(p/(1-p))
	#If you leave the logit, a one-year increase in age decreases 	the log odds of surviving by 0.07
	#As you can see, this interpretation isn't very intuitive

#We can take the logit and get p directly to get the probability of surviving for a one-year increase

#Using the mod2 coefficients our model is log(p/(1-p)) = 1.82 - 0.07*Age
	#Use simple algebra to isolate p and get that p = exp(1.82 - 	0.07*Age) / 1 + exp(1.82 - 0.07*Age)

#For a 1 year increase in age, the odds of survival is exp(-0.07) times the odds of not surviving	
	#At Age = 0, the probability of surviving is .86	 
	#exp(-0.07) = .93 so for every 1 year increase in age, you are 	7% less likely to survive

#Can also interpret in terms of odds ratios
	#Odds ratio: probability of surviving/probability of dying
	#For example, for a 50-year old, the odds of surviving to dying 	is 	exp(1.82 - 0.07*50) = .19, so for a 50-year old, you are 	81% more likely to die than survive

#Let's add Gender to the model
mod3 = glm(Survival ~ Age + Sex, family = "binomial", data = donner)

coef(mod3)
	#log(p/(1-p)) = 3.23 - 0.08*Age - 1.60*Sex
	
	#For a female at Age = 0, the odds of surviving to the odds of 	dying is exp(3.23) = 25.3 (you're 25 more likely to survive 	than die if you are a female at age = 0)
		#The probability of surviving for a female at age = 0 is 		exp(3.23)/(1+exp(3.23) = .96)
		
	#Effect of Age: for a one-year increase in age, the odds of 	surviving are multiplied by .92 - Each year increase in age 	will decrease the chance of surviving, ceteris paribus
	
	#Effect of Sex: exp(-1.60) = .20, so odds for males is .20 		times the odds for females. Males are around 80% less likely to 	survive than females, ceteris paribus
	
	#For a 50 year old male, the odds of surviving to the odds of 	dying are exp(3.23 - 0.08*50 - 1.60) = 0.09 (A 50 year old is 	around 91% more likely to die than survive)

#Was including Sex a significant predictor? Is mod3 better than mod2?
AIC(mod2);AIC(mod3)	#AIC favors mod3

BIC(mod2);BIC(mod3)	#BIC favors mod3

anova(mod2,mod3) #LRT for logistic regression will not provide you with a p-value. Need to compute the p-value using R.
pchisq(5.03, 1, lower.tail = FALSE) #<.05 so we favor mod3

#All three fit statistics favor mod3 so we conclude that including Sex is better

#What if we want to plot the probabilities of surviving as a function of Age?

donner$Survival = as.integer(donner$Survival)

plot(Survival ~ Age, xlab = "Age", ylab = "Survival", main = "Probability of Surviving as Function of Age", data = donner)
points(Survival[Sex == "1"] ~ Age[Sex == "1"], pch = 16, col = "dark blue", data = donner)
points(Survival[Sex == "0"] ~ Age[Sex == "0"], pch = 16, col = "hot pink", data = donner)
c = coef(mod3)
curve(exp(c[1]+c[2]*x)/(1+exp(c[1]+c[2]*x)), add = TRUE, lty = 3, col = "hot pink")
curve(exp(c[1]+c[2]*x+c[3])/(1+exp(c[1]+c[2]*x+c[3])), add = TRUE, lty = 2, col = "dark blue")
legend(50, 0.8, c("Female", "Male"), lty = c(3,2), col = c("hot pink", "dark blue"))

#And that's it, go do your final