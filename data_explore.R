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

cars <- data.frame(mtcars)

all_fit <- lm(mpg ~ . , data = cars)
all_fit_coef <- summary(all_fit)$coef
print(kable(all_fit_coef, caption="All Fit Coefficients"))

# [, 9]	am	    Transmission (0 = automatic, 1 = manual)
am_fit <- lm(mpg ~ am, data = cars)
am_fit_coef <- summary(am_fit)$coef
print(kable(am_fit_coef, caption="All Fit Coefficients"))

# [,10]	gear	Number of forward gears
gr_fit <- lm(mpg ~ gear, data = cars)
gr_fit_coef <- summary(gr_fit)$coef
print(kable(gr_fit_coef, caption="Numb. Gears Coefficients"))

# [, 2]	cyl	    Number of cylinders
cyl_fit <- lm(mpg ~ cyl, data = cars)
cyl_fit_coef <- summary(cyl_fit)$coef
print(kable(cyl_fit_coef, caption="Numb. Cylinders Coefficients"))

# [, 3]	disp	Displacement (cu.in.)
disp_fit <- lm(mpg ~ am + gear, data = cars)
disp_fit_coef <- summary(disp_fit)$coef
print(kable(disp_fit_coef, caption="Displacement Coefficients"))

# [, 4]	hp	    Gross horsepower
hp_fit <- lm(mpg ~ hp, data = cars)
hp_fit_coef <- summary(hp_fit)$coef
print(kable(hp_fit_coef, caption="Horsepower Coefficients"))

# [, 5]	drat	Rear axle ratio
drat_fit <- lm(mpg ~ drat, data = cars)
drat_fit_coef <- summary(drat_fit)$coef
print(kable(drat_fit_coef, caption="Rear Gear Ratio Coefficients"))

# [, 6]	wt	    Weight (lb/1000)
wt_fit <- lm(mpg ~ wt, data = cars)
wt_fit_coef <- summary(wt_fit)$coef
print(kable(wt_fit_coef, caption="Weight Coefficients"))

# [, 7]	qsec	1/4 mile time
qsec_fit <- lm(mpg ~ qsec, data = cars)
qsec_fit_coef <- summary(qsec_fit)$coef
print(kable(qsec_fit_coef, caption="Quarter Mile Time Coefficients"))

# [, 8]	vs	    V/S
vs_fit <- lm(mpg ~ vs, data = cars)
vs_fit_coef <- summary(vs_fit)$coef
print(kable(vs_fit_coef, caption="Vee vs Straight Coefficients"))

# [,11]	carb	Number of carburetors
carb_fit <- lm(mpg ~ carb, data = cars)
carb_fit_coef <- summary(carb_fit)$coef
print(kable(carb_fit_coef, caption="Numb. Carburator Coefficients"))

g_am = ggplot(cars, aes(x = am, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Automatic or Manual") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = am),size = 5, colour = "blue", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_am)

g_wt = ggplot(cars, aes(x = wt, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Weight lb/1000") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = wt),size = 5, colour = "red", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_wt)

g_disp = ggplot(cars, aes(x = disp, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Engine Displacement") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = disp),size = 5, colour = "yellow", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_disp)

g_hp = ggplot(cars, aes(x = hp, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Horsepower") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = hp),size = 5, colour = "green", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_hp)

g_gr = ggplot(cars, aes(x = gear, y =mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Numb. Gears") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = gear),size = 5, colour = "orange", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_gr)

g_carb = ggplot(cars, aes(x = carb, y =mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Numb. Carburators") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = carb),size = 5, colour = "purple", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_carb)

g_drat = ggplot(cars, aes(x = drat, y =mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Rear Axle Ratio") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = drat),size = 5, colour = "pink", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_drat)

g_vs = ggplot(cars, aes(x = vs, y =mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Vee or Straight") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = vs),size = 5, colour = "cyan", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_vs)

g_cyl = ggplot(cars, aes(x = cyl, y =mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Numb. Cylinders") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = cyl),size = 5, colour = "magenta", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(g_cyl)

qsec_cyl = ggplot(cars, aes(x = qsec, y =mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Quarter Mile Time (sec)") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x = qsec),size = 5, colour = "tan", alpha=0.6) +
    geom_smooth(method = "lm", colour = "black") 
print(qsec_cyl)
