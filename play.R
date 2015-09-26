# Automatic vs Manual distributions

fig <- 0
tbl <- 0
data("mtcars")
require(car)
require(ggplot2)
require(dplyr)
require(knitr)

# calculate standard error
se <- function(x) sqrt(var(x)/length(x))

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

car_df <- data.frame(mtcars)
car_df$cyl <- as.factor(car_df$cyl)
car_df$vs <- as.factor(car_df$vs)
car_df$am <- as.factor(car_df$am)
car_df$gear <- as.factor(car_df$gear)
car_df$carb <- as.factor(car_df$carb)

levels(car_df$vs)[1] <- "vee"
levels(car_df$vs)[2] <- "Straight"
levels(car_df$am)[1] <- "Automatic"
levels(car_df$am)[2] <- "Manual"

######## START STUFF ###########################

am_stat <- car_df %>%
    group_by(am) %>%
    summarise_each(funs(mean, sd, se), mpg) %>%
    rename( mpg_mn = mean, mpg_sd = sd, mpg_se = se )

all_fit <- lm(mpg ~ . , data = car_df)
all_fit_coef <- summary(all_fit)$coef
print(kable(all_fit_coef, 
    caption="All Fit Coefficients")
)

all_cor <- cor(mtcars)
mpg_cor <- matrix(mpg, nrow=11, ncol=1, dimnames = list(rownames(all_cor),"mpg"))

nonfact_df <- car_df %>% select(one_of("mpg","wt","disp","hp","drat"))
nonfact_fit <- lm(mpg ~ wt + disp + hp + drat , data = car_df)
nonfact_fit_coef <- summary(nonfact_fit)$coef
print(kable(nonfact_fit_coef, 
    caption="Milage to Weight, Displacement, Horsepower, and Rear-Axle Ratio Coefficients")
)

fact_df <- car_df %>% select(one_of("mpg","am","gear","cyl","vs","carb" ))
fact_fit <- lm(mpg ~ am + gear + cyl + vs + carb , data = car_df)
fact_fit_coef <- summary(fact_fit)$coef
print(kable(fact_fit_coef, 
    caption="Milage to Transmission Type, Gear Count, Cylinder Count, Carburator Count and Cylinder Arrangment Coefficients")
)

am_fit <- lm(mpg ~ am, data = car_df)
am_fit_coef <- summary(am_fit)$coef
print(kable(am_fit_coef, 
    caption="Automatic vs Manual Coefficients")
)

am_glm <- glm(mpg ~ am, data=car_df, family="quasipoisson")
#print(summary(glm(am_glm)))

# mpg_am_pl = ggplot(car_df, aes(x = mpg, fill = am)) + 
#     xlab("Miles per gallon") + 
#     ylab("Count of car_df") + 
#     geom_histogram(binwidth = 1) + 
#     #scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
#     facet_wrap( am ~ . )
#     
# print(mpg_am_pl)

mpg_wt_pl = ggplot(nonfact_df, aes(x = mpg, y = wt)) + 
    ylab("Miles per gallon") + 
    xlab("Weight (1/1000 lb)") + 
    geom_point() +
    geom_smooth(method = "lm", colour = "black")
print(mpg_wt_pl)    

mpg_disp_pl = ggplot(nonfact_df, aes(x = mpg, y = disp)) + 
    ylab("Miles per gallon") + 
    xlab("Displacement (cu. in.)") + 
    geom_point() +
    geom_smooth(method = "lm", colour = "black")
print(mpg_disp_pl)
# + 
#     geom_point(aes(y = disp)) + 
#     geom_point(aes(y = hp)) + 
#     geom_point(aes(y = drat)) + 
#     facet_wrap(mpg~.)







# g_am = ggplot(car_df, aes(x = factor(cyl), y = mpg)) + 
#     geom_smooth(method = "lm", aes(group = 1), colour = "black") +
#     geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
#     geom_point(aes(x=factor(cyl)),size = 5, colour = "blue", alpha=0.6) +
#     ylab("Miles per gallon") + 
#     xlab("Number of Cylinders")  +
#     scale_manual(breaks=c(4,6,8), values=c("4 Cyl","6 Cyl", "8 Cyl"))
# print(g_am)

# g_am = ggplot(car_df, aes(x = am, y = mpg)) + 
#     ylab("Miles per gallon") + 
#     xlab("Automatic or Manual") + 
#     geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
#     geom_point(aes(x=am),size = 5, colour = "blue", alpha=0.6) +    
#     scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
#     geom_smooth(method = "lm", colour = "black") 
# print(g_am)

# log_am = ggplot(car_df, aes(x = am, y = log(mpg))) + 
#     ylab("Miles per gallon (log)") + 
#     xlab("Automatic or Manual") + 
#     geom_point(aes(y=log(mpg)), size = 7, colour = "black", alpha=0.5) + 
#     geom_point(aes(x=am),size = 5, colour = "blue", alpha=0.6) +
#     scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
#     geom_smooth(method = "lm", colour = "black") 
# print(log_am)

# am_resid <- resid(am_fit)
# amres_pl <- ggplot(car_df, aes(x=am, y=am_resid)) +
#             ylab("Residuals") + 
#             xlab("Automatic or Manual") +
#             geom_hline(yintercept=0) +
#             geom_point(aes(y=am_resid), size = 7, colour = "red", alpha=0.5) + 
#             geom_point(aes(x=am),size = 5, colour = "red", alpha=0.6) +
#             scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual"))
# print(amres_pl)

