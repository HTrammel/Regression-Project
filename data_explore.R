#########################################
# Car Data Exploration
#----------------------------------------------------------
# 
# A data frame with 32 observations on 11 variables.
# 
# [, 1]	mpg	    Miles/(US) gallon
# [, 2]	cyl	    Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	    Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	    Weight (lb/1000)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	    V/S
# [, 9]	am	    Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors
#----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(knitr)

data("mtcars")

carDF <- data.frame(mtcars)

fit <- lm(mpg ~ am, carDF)
fit2 <- update(fit, mpg ~ am + wt)
fit3 <- update(fit, mpg ~ am + wt + carb)
fit4 <- update(fit, mpg ~ am + wt + carb + hp)
fit5 <- update(fit, mpg ~ am + wt + carb + hp + cyl)
fit6 <- update(fit, mpg ~ am + wt + carb + hp + cyl + drat)

print(anova(fit, fit2, fit3, fit4, fit5, fit6))

wfit <-            lm(mpg ~ wt, carDF)
wfit2 <- update(wfit, mpg ~ wt + cyl)
wfit3 <- update(wfit, mpg ~ wt + cyl + carb)
wfit4 <- update(wfit, mpg ~ wt + cyl + carb + am)
wfit5 <- update(wfit, mpg ~ wt + cyl + carb + am + hp)
wfit6 <- update(wfit, mpg ~ wt + cyl + carb + am + hp + disp)

print(anova(wfit, wfit2, wfit3, wfit4, wfit5, wfit6))

dfit <-            lm(mpg ~ wt, carDF)
dfit2 <- update(dfit, mpg ~ wt + hp)
dfit3 <- update(dfit, mpg ~ wt + hp + cyl)
dfit4 <- update(dfit, mpg ~ wt + hp + cyl + carb)
dfit5 <- update(dfit, mpg ~ wt + hp + cyl + carb + am)
dfit6 <- update(dfit, mpg ~ wt + hp + cyl + carb + am + drat)

print(anova(dfit, dfit2, dfit3, dfit4, dfit5, dfit6))

wfit <-            lm(mpg ~ wt, carDF)
wfit2 <- update(wfit, mpg ~ wt + carb)
wfit3 <- update(wfit, mpg ~ wt + carb + cyl)
wfit4 <- update(wfit, mpg ~ wt + carb + cyl + am)
wfit5 <- update(wfit, mpg ~ wt + carb + cyl + am + hp)
wfit6 <- update(wfit, mpg ~ wt + carb + cyl + am + hp + disp)

print(anova(wfit, wfit2, wfit3, wfit4, wfit5, wfit6))

