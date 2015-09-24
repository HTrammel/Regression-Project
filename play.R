# Automatic vs Manual distributions

fig <- 0
tbl <- 0

data("mtcars")
require(car)
require(pastecs)
require(ggplot2)
require(dplyr)
require(knitr)

desc_stat_lbl <- c("number of values (nbr.val) "
                    ,"number of null values (nbr.null) "
                    ,"number of missing values (nbr.na)"
                    ,"minimal value (min)"
                    ,"maximal value (max)"
                    ,"range (range that is max-min)"
                    ,"sum of all non-missing values (sum)"
                    ,"median (median)"
                    ,"mean (mean)"
                    ,"standard error on mean (SE.mean) "
                    ,"confidence interval of mean (CI.mean)"
                    ,"variance (var) "
                    ,"standard deviation (std.dev) "
                    ,"variation coefficient (coef.var)") 

cars <- data.frame(mtcars)
cars$cyl <- as.factor(cars$cyl)
cars$vs <- as.factor(cars$vs)
cars$am <- as.factor(cars$am)
cars$gear <- as.factor(cars$gear)
cars$carb <- as.factor(cars$carb)

######## START STUFF ###########################
stat_table <- data.frame(matrix(data=1:42, nrow=14, ncol=1))
stat_table[,1] <- desc_stat_lbl
stat_table <- cbind(stat_table, stat.desc(auto_df), stat.desc(manual_df))
names(stat_table) <- c("Statistic", "Automatic", "Manual")


all_fit <- lm(mpg ~ . , data = cars)
all_fit_coef <- summary(all_fit)$coef
print(kable(all_fit_coef, caption="All Fit Coefficients"))

nonfact_fit <- lm(mpg ~ wt + disp + drat , data = cars)
nonfact_fit_coef <- summary(nonfact_fit)$coef
print(kable(nonfact_fit_coef, caption="Non-factor Fit Coefficients"))

fact_fit <- lm(mpg ~ am + gear + cyl + vs + carb , data = cars)
fact_fit_coef <- summary(fact_fit)$coef
print(kable(fact_fit_coef, caption="Factor Fit Coefficients"))

am_fit <- lm(mpg ~ am, data = cars)
am_fit_coef <- summary(am_fit)$coef
print(kable(am_fit_coef, caption="Automatic vs Manual Coefficients"))

auto_df <- cars %>% 
            filter(am == 0) %>% 
            select(one_of(c("mpg")))
manual_df <- cars %>% 
            filter(am == 1) %>% 
            select(one_of(c("mpg")))


# digits set to vector did not work
# print(kable(stat_table, digits = c(0,0,0,2,2,2,2,2,4,4,4,4,4,4), row.names=F))
print(kable(stat_table, digits = getOption("digits"), row.names=F))

am_glm <- glm(mpg ~ am, data=cars, family="quasipoisson")
#print(summary(glm(am_glm)))



g_am = ggplot(cars, aes(x = factor(cyl), y = mpg)) + 
    geom_smooth(method = "lm", aes(group = 1), colour = "black") +
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x=factor(cyl)),size = 5, colour = "blue", alpha=0.6) +
    ylab("Miles per gallon") + 
    xlab("Number of Cylinders")  +
    scale_manual(breaks=c(4,6,8), values=c("4 Cyl","6 Cyl", "8 Cyl"))
print(g_am)

# g_am = ggplot(cars, aes(x = am, y = mpg)) + 
#     ylab("Miles per gallon") + 
#     xlab("Automatic or Manual") + 
#     geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
#     geom_point(aes(x=am),size = 5, colour = "blue", alpha=0.6) +    
#     scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
#     geom_smooth(method = "lm", colour = "black") 
# print(g_am)

# log_am = ggplot(cars, aes(x = am, y = log(mpg))) + 
#     ylab("Miles per gallon (log)") + 
#     xlab("Automatic or Manual") + 
#     geom_point(aes(y=log(mpg)), size = 7, colour = "black", alpha=0.5) + 
#     geom_point(aes(x=am),size = 5, colour = "blue", alpha=0.6) +
#     scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
#     geom_smooth(method = "lm", colour = "black") 
# print(log_am)

# am_resid <- resid(am_fit)
# amres_pl <- ggplot(cars, aes(x=am, y=am_resid)) +
#             ylab("Residuals") + 
#             xlab("Automatic or Manual") +
#             geom_hline(yintercept=0) +
#             geom_point(aes(y=am_resid), size = 7, colour = "red", alpha=0.5) + 
#             geom_point(aes(x=am),size = 5, colour = "red", alpha=0.6) +
#             scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual"))
# print(amres_pl)

