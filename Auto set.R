#imports
library(ggplot2)
library(MASS)
library(dplyr)
library(car)
library(stats) 
library(knitr)


#Import Data
auto = Auto

#Explore Data
str(auto)
summary(auto)
colnames(auto)
head(auto, n=10)


#Explore Data
plot(auto)
#Relationship between mpg and displacement, horsepower, weight, year
ggplot(auto, aes(x = mpg, y = weight)) + geom_point(aes(color = cylinders))
#Heavier cars have more cylinders, lighter vehicles have less cylinders and give more mpg
ggplot(auto, aes(x = year, y = mpg)) + geom_point(aes(color = weight))
#over a short span of 12 years, the weight of the cars has reduced by approximately 3000lbs, and mpg has shifted from a maximum of 27mpg to 47mpg
autoModel1 = lm(mpg ~ cylinders + horsepower + weight + displacement + year + acceleration, data = auto)
summary(autoModel1)
#weight and year are very significant.

#Determine coliniarity 
fitvif <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+year, data = auto)
show(vif(fitvif))
#displacement has the highest VIF (above ~10)

#variable selection
#using stepwise selection
fit <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+year, data = auto)
step <- stepAIC(fit, direction="both", trace=FALSE)
summary(step)$coeff
summary(step)$r.squared
#shows adjusted R^2 to be 80%, meaning weight and year explain 80% of the variation in mpg. (Adequate model)

#test each parameter via nested likelihood ratio test
fit1 <- lm(mpg ~ weight, data = auto)
fit2 <- lm(mpg ~ weight+year, data = auto)
fit3 <- lm(mpg ~ weight+year+cylinders, data = auto)
fit4 <- lm(mpg ~ weight+year+cylinders+horsepower, data = auto)
fit5 <- lm(mpg ~ weight+year+cylinders+horsepower+acceleration, data = auto)
anova(fit1, fit2, fit3, fit4, fit5)
#note the spike in sum of squares when we run fit2 (weight + year)

#final Model
finalfit <- lm(mpg ~ weight+year, data = auto)
summary(finalfit)$coef

#detect colliniarity 
fitvif <- lm(mpg ~ weight+year, data = auto)
show(vif(fitvif))
#we are okay ( no values above ~10)


#residual plot
par(mfrow=c(2,2))
plot(fitvif)
#The visibility of a distinct pattern in our residual plot indicates that further transformation can be done on our dataset.

