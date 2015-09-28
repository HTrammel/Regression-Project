#########################################
# Car Data Exploration
#----------------------------------------------------------


data("mtcars")

carDF <- data.frame(mtcars)

fit <- lm(mpg ~ am, carDF)
fit2 <- update(fit, mpg ~ am + wt)
fit3 <- update(fit, mpg ~ am + wt + carb)
fit4 <- update(fit, mpg ~ am + wt + carb + hp)
fit5 <- update(fit, mpg ~ am + wt + carb + hp + cyl)
fit6 <- update(fit, mpg ~ am + wt + carb + hp + cyl + drat)

print(anova(fit, fit2, fit3, fit4, fit5, fit6))

# wfit <-            lm(mpg ~ wt, carDF)
# wfit2 <- update(wfit, mpg ~ wt + cyl)
# wfit3 <- update(wfit, mpg ~ wt + cyl + carb)
# wfit4 <- update(wfit, mpg ~ wt + cyl + carb + am)
# wfit5 <- update(wfit, mpg ~ wt + cyl + carb + am + hp)
# wfit6 <- update(wfit, mpg ~ wt + cyl + carb + am + hp + disp)
# 
# print(anova(wfit, wfit2, wfit3, wfit4, wfit5, wfit6))
# 
# dfit <-            lm(mpg ~ wt, carDF)
# dfit2 <- update(dfit, mpg ~ wt + hp)
# dfit3 <- update(dfit, mpg ~ wt + hp + cyl)
# dfit4 <- update(dfit, mpg ~ wt + hp + cyl + carb)
# dfit5 <- update(dfit, mpg ~ wt + hp + cyl + carb + am)
# dfit6 <- update(dfit, mpg ~ wt + hp + cyl + carb + am + drat)
# 
# print(anova(dfit, dfit2, dfit3, dfit4, dfit5, dfit6))
# 
# wfit <-            lm(mpg ~ wt, carDF)
# wfit2 <- update(wfit, mpg ~ wt + carb)
# wfit3 <- update(wfit, mpg ~ wt + carb + cyl)
# wfit4 <- update(wfit, mpg ~ wt + carb + cyl + am)
# wfit5 <- update(wfit, mpg ~ wt + carb + cyl + am + hp)
# wfit6 <- update(wfit, mpg ~ wt + carb + cyl + am + hp + disp)
# 
# print(anova(wfit, wfit2, wfit3, wfit4, wfit5, wfit6))
# 
