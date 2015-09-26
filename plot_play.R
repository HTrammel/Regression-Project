# Automatic vs Manual distributions

fig <- 0
tbl <- 0
data("mtcars")
require(car)
require(pastecs)
require(ggplot2)
require(dplyr)
require(knitr)

# calculate standard error
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

######## START STUFF ###########################

impactDF <- carDF %>% select( one_of ("mpg","am","disp","hp","wt","drat"))

tbl <- tbl + 1
mpg_wt_pl = ggplot(carDF, aes(x = wt, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Weight (1/1000 lb)") + 
    ggtitle(paste("Table", tbl, "Weight Impact on Mileage")) +
    guides(colour = guide_legend(title="Transmission Type")) +
    geom_point(aes(colour=am), size=4) +
    geom_smooth(method = "lm", colour = "black")
print(mpg_wt_pl)    

tbl <- tbl + 1
mpg_disp_pl = ggplot(carDF, aes(x = disp, y = mpg )) + 
    ylab("Miles per gallon") + 
    xlab("Displacement (cu. in.)") + 
    ggtitle(paste("Table", tbl, "Displacement Impact on Mileage")) +
    guides(colour = guide_legend(title="Transmission Type")) +
    geom_point(aes(colour=am), size=4) +
    geom_smooth(method = "lm", colour = "black")
print(mpg_disp_pl)

tbl <- tbl + 1
mpg_hp_pl = ggplot(carDF, aes(x = hp, y = mpg )) + 
    ylab("Miles per gallon") + 
    xlab("Horsepower") + 
    ggtitle(paste("Table", tbl, "Horsepower Impact on Mileage")) +
    guides(colour = guide_legend(title="Transmission Type")) +
    geom_point(aes(colour=am), size=4) +
    geom_smooth(method = "lm", colour = "black")
print(mpg_hp_pl)

tbl <- tbl + 1
mpg_drat_pl = ggplot(carDF, aes(x = drat, y = mpg )) + 
    ylab("Miles per gallon") + 
    xlab("Rear Axle Ratio") + 
    ggtitle(paste("Table", tbl, "Rear Axle Ratio Impact on Mileage")) +
    guides(colour = guide_legend(title="Transmission Type")) +
    geom_point(aes(colour=am), size=4) +
    geom_smooth(method = "lm", colour = "black")
print(mpg_drat_pl)

am_fit <- lm(mpg ~ am, data = carDF)
am_fit_coef <- summary(am_fit)$coef
kable(am_fit_coef, caption=paste("Table", tbl, "Automatic vs Manual Coefficients"))
tbl <- tbl + 1

am_e <- resid(am_fit)
e_am_pl = ggplot(carDF, aes(x = am, y=am_e, fill = am)) + 
    geom_boxplot() +     
    ylab("Residuals (mpg)") + 
    xlab("Transmission type") +
    ggtitle(paste("Figure", fig, "Residuals of Transmission vs Mileage")) +
    guides(colour = guide_legend(title="Transmission")) 
print(e_am_pl)
fig <- fig + 1

# tbl <- tbl + 1
# mpg_cyl_pl = ggplot(carDF, aes(x = cyl, y = mpg )) + 
#     ylab("Miles per gallon") + 
#     xlab("Number of Cylinders") + 
#     ggtitle(paste("Table", tbl, "Cylinder Count Impact on Mileage")) +
#     guides(colour = guide_legend(title="Transmission Type")) +
#     geom_point(aes(colour=am), size=4) +
#     geom_smooth(method = "lm", colour = "black")
# print(mpg_cyl_pl)


# mpg_am_pl = ggplot(carDF, aes(x = mpg, fill = am)) + 
#     xlab("Miles per gallon") + 
#     ylab("Count of carDF") + 
#     geom_histogram(binwidth = 1) + 
#     #scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
#     facet_wrap( am ~ . )
    
# print(mpg_am_pl)





# g_am = ggplot(carDF, aes(x = factor(cyl), y = mpg)) + 
#     geom_smooth(method = "lm", aes(group = 1), colour = "black") +
#     geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
#     geom_point(aes(x=factor(cyl)),size = 5, colour = "blue", alpha=0.6) +
#     ylab("Miles per gallon") + 
#     xlab("Number of Cylinders")  +
#     scale_manual(breaks=c(4,6,8), values=c("4 Cyl","6 Cyl", "8 Cyl"))
# print(g_am)

# g_am = ggplot(carDF, aes(x = am, y = mpg)) + 
#     ylab("Miles per gallon") + 
#     xlab("Automatic or Manual") + 
#     geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
#     geom_point(aes(x=am),size = 5, colour = "blue", alpha=0.6) +    
#     scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
#     geom_smooth(method = "lm", colour = "black") 
# print(g_am)

# log_am = ggplot(carDF, aes(x = am, y = log(mpg))) + 
#     ylab("Miles per gallon (log)") + 
#     xlab("Automatic or Manual") + 
#     geom_point(aes(y=log(mpg)), size = 7, colour = "black", alpha=0.5) + 
#     geom_point(aes(x=am),size = 5, colour = "blue", alpha=0.6) +
#     scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
#     geom_smooth(method = "lm", colour = "black") 
# print(log_am)

# am_resid <- resid(am_fit)
# amres_pl <- ggplot(carDF, aes(x=am, y=am_resid)) +
#             ylab("Residuals") + 
#             xlab("Automatic or Manual") +
#             geom_hline(yintercept=0) +
#             geom_point(aes(y=am_resid), size = 7, colour = "red", alpha=0.5) + 
#             geom_point(aes(x=am),size = 5, colour = "red", alpha=0.6) +
#             scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual"))
# print(amres_pl)



#### Figure `r fig`: Weight Impact on Mileage
   


mpg_wt_pl = ggplot(carDF, aes(x = wt, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Weight (1/1000 lb)") + 
    guides(colour = guide_legend(title="Transmission")) +
    geom_point(aes(colour=am), size=2) +
    geom_smooth(method = "lm", colour = "black")
print(mpg_wt_pl)    
fig <- fig + 1



#### Figure `r fig`: Displacement Impact on Mileage



mpg_disp_pl = ggplot(carDF, aes(x = disp, y = mpg )) + 
    ylab("Miles per gallon") + 
    xlab("Displacement (cu. in.)") + 
    guides(colour = guide_legend(title="Transmission")) +
    geom_point(aes(colour=am), size=2) +
    geom_smooth(method = "lm", colour = "black")
print(mpg_disp_pl)
fig <- fig + 1




#### Figure `r fig`: Horsepower Impact on Mileage


mpg_hp_pl = ggplot(carDF, aes(x = hp, y = mpg )) + 
    ylab("Miles per gallon") + 
    xlab("Horsepower") + 
    guides(colour = guide_legend(title="Transmission")) +
    geom_point(aes(colour=am), size=2) +
    geom_smooth(method = "lm", colour = "black")
print(mpg_hp_pl)
fig <- fig + 1



#### Figure `r fig`: Rear Axle Ratio Impact on Mileage


mpg_drat_pl = ggplot(carDF, aes(x = drat, y = mpg )) + 
    ylab("Miles per gallon") + 
    xlab("Rear Axle Ratio") + 
    guides(colour = guide_legend(title="Transmission")) +
    geom_point(aes(colour=am), size=2) +
    geom_smooth(method = "lm", colour = "black")
print(mpg_drat_pl)
fig <- fig + 1


qqPlot(lm(mpg ~ am, mtcars))

qqPlot(lm(mpg ~ am, carDF))