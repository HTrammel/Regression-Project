# Automatic vs Manual distributions

data("mtcars")
require(car)
require(ggplot2)
require(dplyr)
require(knitr)


am_fit <- lm(mpg ~ am, data = mtcars)
am_fit_coef <- summary(am_fit)$coef
print(kable(am_fit_coef, caption="Automatic vs Manual Coefficients"))


avm_pl <- ggplot(mtcars) +
        geom_histogram(binwidth=1, aes(x=mpg, fill=factor(am))) +
        facet_grid(am ~ .)

print(avm_pl)

auto_df <- mtcars %>% 
    filter(am == 0) %>% 
    select(one_of(c("mpg")))
auto_sum <- summary(auto_df, responseName="Automatic MPG")

manual_df <- mtcars %>% 
    filter(am == 1) %>% 
    select(one_of(c("mpg")))
manual_sum <- summary(manual_df, responseName="Manual MPG")

ss <- cbind(auto_sum, manual_sum)

print(kable(ss, col.names = c("Automatic MPG","Manual MPG"), align="l"))

g_am = ggplot(mtcars, aes(x = am, y = mpg)) + 
    ylab("Miles per gallon") + 
    xlab("Automatic or Manual") + 
    geom_point(aes(y=mpg), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x=am),size = 5, colour = "blue", alpha=0.6) +    
    scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
    geom_smooth(method = "lm", colour = "black") 
print(g_am)

log_am = ggplot(mtcars, aes(x = am, y = log(mpg))) + 
    ylab("Miles per gallon (log)") + 
    xlab("Automatic or Manual") + 
    geom_point(aes(y=log(mpg)), size = 7, colour = "black", alpha=0.5) + 
    geom_point(aes(x=am),size = 5, colour = "blue", alpha=0.6) +
    scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual")) + 
    geom_smooth(method = "lm", colour = "black") 
print(log_am)

am_resid <- resid(am_fit)
amres_pl <- ggplot(mtcars, aes(x=am, y=am_resid)) +
            ylab("Residuals") + 
            xlab("Automatic or Manual") +
            geom_hline(yintercept=0) +
            geom_point(aes(y=am_resid), size = 7, colour = "red", alpha=0.5) + 
            geom_point(aes(x=am),size = 5, colour = "red", alpha=0.6) +
            scale_x_continuous(breaks=0:1, labels=c("Automatic","Manual"))
print(amres_pl)

#hist(am_resid)

am_glm <- glm(mpg ~ am, data=mtcars, family="quasipoisson")
print(summary(glm(am_glm)))

