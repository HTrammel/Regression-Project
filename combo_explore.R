#########################################
# Car Data Exploration
#----------------------------------------------------------
require(pastecs)
require(ggplot2)
require(dplyr)
require(knitr)

carDF <- data.frame(mtcars)
carDF$cyl <- as.factor(carDF$cyl)
carDF$vs <- as.factor(carDF$vs)
carDF$am <- as.factor(carDF$am)
carDF$gear <- as.factor(carDF$gear)
carDF$carb <- as.factor(carDF$carb)

levels(carDF$vs)[1] <- "vee"
levels(carDF$vs)[2] <- "Straight"
levels(carDF$am)[1] <- "Automatic"
levels(carDF$am)[2] <- "Manual"

am_fit <- lm(mpg ~ am, data = carDF)
am_fit_coef <- summary(am_fit)$coef
print(kable(am_fit_coef, caption="Automatic vs Manual Coefficients"))

wt_fit <- lm(mpg ~ wt, data = carDF)
wt_fit_coef <- summary(wt_fit)$coef
print(kable(wt_fit_coef, caption="Weight Coefficients"))

wt_am <- lm(mpg ~ wt + am, data = carDF)
wt_am_coef <- summary(wt_am)$coef
print(kable(wt_am_coef, caption="Weight:Trans Coefficients"))

disp_fit <- lm(mpg ~ disp, data = carDF)
disp_fit_coef <- summary(disp_fit)$coef
print(kable(disp_fit_coef, caption="Displacement Coefficients"))

disp_am <- lm(mpg ~ disp + am, data = carDF)
disp_am_coef <- summary(disp_am)$coef
print(kable(disp_am_coef, caption="Displacement:Trans Coefficients"))

hp_fit <- lm(mpg ~ hp, data = carDF)
hp_fit_coef <- summary(hp_fit)$coef
print(kable(hp_fit_coef, caption="Horsepower Coefficients"))

hp_am <- lm(mpg ~ hp + am, data = carDF)
hp_am_coef <- summary(hp_am)$coef
print(kable(hp_am_coef, caption="Horsepower:Trans Coefficients"))

drat_fit <- lm(mpg ~ drat, data = carDF)
drat_fit_coef <- summary(drat_fit)$coef
print(kable(drat_fit_coef, caption="Rear Gear Ratio Coefficients"))

drat_am <- lm(mpg ~ drat + am, data = carDF)
drat_am_coef <- summary(drat_am)$coef
print(kable(drat_am_coef, caption="Rear Gear Ratio:Trans Coefficients"))

carb_fit <- lm(mpg ~ carb, data = carDF)
carb_fit_coef <- summary(carb_fit)$coef
print(kable(carb_fit_coef, caption="Numb. Carbs Coefficients"))

carb_am <- lm(mpg ~ carb + am, data = carDF)
carb_am_coef <- summary(carb_am)$coef
print(kable(carb_am_coef, caption="Numb. Carbs:Trans Coefficients"))

cyl_fit <- lm(mpg ~ cyl, data = carDF)
cyl_fit_coef <- summary(cyl_fit)$coef
print(kable(cyl_fit_coef, caption="Numb. Cylinders Coefficients"))

cyl_am <- lm(mpg ~ cyl + am, data = carDF)
cyl_am_coef <- summary(cyl_am)$coef
print(kable(cyl_am_coef, caption="Numb. Cylinders:Trans Coefficients"))

gr_fit <- lm(mpg ~ gear, data = carDF)
gr_fit_coef <- summary(gr_fit)$coef
print(kable(gr_fit_coef, caption="Numb. Gears Coefficients"))

gear_am <- lm(mpg ~ gear + am, data = carDF)
gear_am_coef <- summary(gear_am)$coef
print(kable(gear_am_coef, caption="Numb. Gears:Trans Coefficients"))










# vs_fit <- lm(mpg ~ vs, data = carDF)
# vs_fit_coef <- summary(vs_fit)$coef
# print(kable(vs_fit_coef, caption="Vee vs Straight Coefficients"))

# qsec_fit <- lm(mpg ~ qsec, data = carDF)
# qsec_fit_coef <- summary(qsec_fit)$coef
# print(kable(qsec_fit_coef, caption="Quarter Mile Time Coefficients"))

# all_cor <- cor(mtcars)
# print(kable(all_cor, digits = 4, caption="Correlation Matrix for All Variables"))

# all_fit <- lm(mpg ~ . , data = carDF)
# all_fit_coef <- summary(all_fit)$coef
# print(kable(all_fit_coef, caption="All Fit Coefficients"))