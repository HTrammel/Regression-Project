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
plot(cdata$mpg ~ cdata$am)

mn_car <- cdata %>% 
            group_by(am) %>% 
            summarise_each(funs(sum, mean, sd, se), mpg)

am_mpg_lm <- lm(cdata$mpg ~ cdata$am)