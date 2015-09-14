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

data("mtcars")
# calculate standard error
se <- function(x) sqrt(var(x)/length(x))

cdata <- mtcars
plot(cdata$mpg ~ cdata$carb)

g1 = ggplot(cdata, aes(x = carb, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Automatic or Manual") + 
    geom_point(size = 7, colour = "black", alpha=0.5) + 
    geom_point(size = 5, colour = "blue", alpha=0.2) + 
    geom_smooth(method = "lm", colour = "black") 
print(g1)

# g2 = ggplot(cdata, aes(x = mpg)) +
#     xlab("Miles per gallon") +
#     geom_point(stat="bin") + 
#     facet_wrap(mpg ~ .)
# print(g2)