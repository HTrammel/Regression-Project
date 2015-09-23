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

require(memisc)
require(pander)
require(ggplot2)
require(dplyr)
require(knitr)

data("mtcars")

cars <- data.frame(mtcars)

# All variables munged
all_fit <- lm(mpg ~ . , data = cars)
all_fit_coef <- summary(all_fit)$coef
print(kable(all_fit_coef, caption="All Fit Coefficients"))

# [, 9]	am	    Transmission (0 = automatic, 1 = manual)
am_fit <- lm(mpg ~ am, data = cars)
am_fit_coef <- summary(am_fit)$coef
print(kable(am_fit_coef, caption="Automatic vs Manual Coefficients"))

# Transmission effects on weight impacted MPG
# [, 6] wt      Weight (lb/1000)
wt_fit <- lm(mpg ~ wt, data = cars)
wt_am <- lm(mpg ~ wt + am, data = cars)
wt_fit_coef <- summary(wt_fit)$coef
wt_am_coef <- summary(wt_am)$coef
print(kable(wt_fit_coef, caption="Weight Coefficients"))
print(kable(wt_am_coef, caption="Weight:Trans Type Coefficients"))

weight_mod <- mtable('Weight_Only' = wt_fit,
            'Weight_and_Transmission' = wt_am,
            summary.stats = c('R-squared','F','p','N'))
pander(weight_mod)

g_wt = ggplot(cars, aes(x = wt, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Weight lb/1000") + 
    geom_point(aes(color=factor(am)), size= 5) + 
    geom_smooth(method = "lm", colour = "black") 
print(g_wt)

# Transmission effects on Engine impacted MPG
# [, 2] cyl     Number of cylinders
# cyl_fit <- lm(mpg ~ cyl, data = cars)
# cyl_fit_coef <- summary(cyl_fit)$coef
#print(kable(cyl_fit_coef, caption="Numb. Cylinders Coefficients"))

# [, 3] disp    Displacement (cu.in.)
# disp_fit <- lm(mpg ~ disp, data = cars)
# disp_fit_coef <- summary(disp_fit)$coef
#print(kable(disp_fit_coef, caption="Displacement Coefficients"))

# [, 4] hp      Gross horsepower
# hp_fit <- lm(mpg ~ hp, data = cars)
# hp_fit_coef <- summary(hp_fit)$coef
#print(kable(hp_fit_coef, caption="Horsepower Coefficients"))

# [, 8] vs      V/S
# vs_fit <- lm(mpg ~ vs, data = cars)
# vs_fit_coef <- summary(vs_fit)$coef
#print(kable(vs_fit_coef, caption="Vee vs Straight Coefficients"))

# [,11] carb    Number of carburetors
# carb_fit <- lm(mpg ~ carb, data = cars)
# carb_fit_coef <- summary(carb_fit)$coef
#print(kable(carb_fit_coef, caption="Numb. Carburator Coefficients"))


# Transmission effects on Drivetrain impacted MPG

# [,10]	gear	Number of forward gears
# gr_fit <- lm(mpg ~ gear, data = cars)
# gr_fit_coef <- summary(gr_fit)$coef
#print(kable(gr_fit_coef, caption="Numb. Gears Coefficients"))

# [, 5]	drat	Rear axle ratio
# drat_fit <- lm(mpg ~ drat, data = cars)
# drat_fit_coef <- summary(drat_fit)$coef
#print(kable(drat_fit_coef, caption="Rear Gear Ratio Coefficients"))



# Transmission effects on Performance impacted MPG
# [, 7]	qsec	1/4 mile time
# qsec_fit <- lm(mpg ~ qsec, data = cars)
# qsec_fit_coef <- summary(qsec_fit)$coef
#print(kable(qsec_fit_coef, caption="Quarter Mile Time Coefficients"))

