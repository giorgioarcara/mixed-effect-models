set.seed(220) # you can change the seed for different results
rm(list=ls())
require(effects)
require(car)
library(emmeans)


## SIMPLE LINEAR REGRESSION ##
Complexity = rnorm(100, mean = 80, sd = 10)
Error = rnorm(100, mean=0, sd = 8)
RT = 200 + 0.5*Complexity + Error

dat1 = data.frame(RT=RT, Complexity=Complexity)

mod1 = lm(RT~Complexity, dat1)
summary(mod1)
#regression1
png("Figures/lm_1.png", height=800, width=800, res=200)
plot(allEffects(mod1))
dev.off()


##### plot scatterplot
png("Figures/lm1_scatter.png", height=800, width=800, res=200)
with(dat1, plot(Complexity, RT))
dev.off()

##### plot scatterplot
png("Figures/lm1_fit.png", height=800, width=800, res=200)
with(dat1, plot(Complexity, RT))
abline(mod1)
dev.off()

##### plot residuals
png("Figures/lm_1_resid1.png", height=800, width=800, res=200)
with(dat1, plot(Complexity, RT))
abline(mod1)
with(dat1, segments(x0=Complexity, y0=fitted(mod1), x1=Complexity, y1=RT, col="darkgray", lty=2))
dev.off()

## MULTIPLE LINEAR REGRESSION ##
Complexity = rnorm(100, mean = 80, sd = 10)
Imageability = rnorm(100, mean = 20, sd = 5)
Error = rnorm(100, mean=0, sd = 8)
RT = 200 + 0.5*Complexity + -1.2*Imageability + Error
dat2 = data.frame(RT=RT, Complexity=Complexity, Imageability=Imageability)
mod2 = lm(RT~Complexity+Imageability, dat2)
summary(mod2)

png("Figures/lm_2.png", height=800, width=1600, res=200)
plot(allEffects(mod2))
dev.off()


## MULTIPLE LINEAR REGRESSION WITH FACTORS ##
PrimeType = rep(c("Positive", "Negative"), each=50)
Complexity = factor(Complexity)
PrimeType_dummy = model.matrix( ~ PrimeType )[,2]
Imageability = rnorm(100, mean = 20, sd = 5)
Error = rnorm(100, mean=0, sd = 8)
RT = 200 + 5*PrimeType_dummy + -2*Imageability + Error
dat3 = data.frame(RT = RT, PrimeType=PrimeType, Imageability = Imageability)

mod3 = lm(RT~PrimeType+Imageability, dat3)
summary(mod3)
Anova(mod3, type="III")


png("Figures/lm_3.png", height=800, width=1600, res=200)
plot(allEffects(mod3))
dev.off()


## INTERACTION BETWEEN FACTORS ##
PrimeType = rep(c("Positive", "Negative"), each=50)
PrimeType = factor(PrimeType)
PrimeType_dummy = model.matrix( ~ PrimeType )[,2]
TaskDiff = rep(c("Easy", "Difficult"), 50)
TaskDiff = factor(TaskDiff)
TaskDiff_dummy = model.matrix( ~ TaskDiff )[,2]

Error = rnorm(100, mean=0, sd = 8)
RT = 200 + 10*PrimeType_dummy + +8*TaskDiff_dummy - 6.5*PrimeType_dummy*TaskDiff_dummy + Error

dat4 = data.frame(RT=RT, PrimeType=PrimeType, TaskDiff=TaskDiff)

mod4 = lm(RT~PrimeType*TaskDiff, dat4)
summary(mod4)

png("Figures/lm_4.png", height=800, width=1600, res=200)
plot(allEffects(mod4))
dev.off()

##############
# POST HOCS
##############

Anova(mod4, type="III")
pairs(emmeans(mod4, ~PrimeType*TaskDiff))



## INTERACTION BETWEEN FACTOR AND CONTINUOUS VARIABLES ##
PrimeType = rep(c("Positive", "Negative"), each=50)
PrimeType = factor(PrimeType)
PrimeType_dummy = model.matrix( ~ PrimeType )[,2]
Imageability = rnorm(100, mean = 20, sd = 5)
Error = rnorm(100, mean=0, sd = 50)
RT = 200 + -40*PrimeType_dummy + -4*Imageability + 5*PrimeType_dummy*Imageability + Error
dat5 = data.frame(RT=RT, PrimeType=PrimeType, Imageability=Imageability)

mod5 = lm(RT~PrimeType*Imageability, dat5)
summary(mod5)

plot(allEffects(mod5))


png("Figures/lm_5.png", height=800, width=1600, res=200)
plot(allEffects(mod5))
dev.off()

Anova(mod5, type="III")

pairs(emmeans(mod5, ~PrimeType)) #misleading
pairs(emmeans(mod5, ~PrimeType | Imageability)) 
Imag_values= quantile(dat5$Imageability, probs = c(0.05, 0.5, 0.90))
pairs(emmeans(mod5, ~PrimeType | Imageability, at = list(Imageability = Imag_values)))



## diagnostics

## linear relationship
png("Figures/diagn_1.png", height=800, width=1600, res=200)
plot(allEffects(mod2, partial.residuals=T))
dev.off()

# homoscedasticity of residuals
png("Figures/diagn_2.png", height=800, width=800, res=200)
plot(fitted(mod2), resid(mod2), xlab="fitted", ylab="residuals")
abline(h=0, lty=2)
dev.off()

# normality of residuals
png("Figures/diagn_3a.png", height=800, width=800, res=200)
hist(resid(mod2), xlab="residuals")
dev.off()

# normality of residuals
png("Figures/diagn_3b.png", height=800, width=800, res=200)
qqnorm(resid(mod2))
qqline(resid(mod2))
dev.off()

# multicollinearity
# note I need at least two predictors
vif(mod3)

#independence of errors
png("Figures/diagn_5.png", height=800, width=800, res=200)
acf(resid(mod2)) 
dev.off()
# this check make sense assuming an autocorrelation, that is the dependency is related to the order in the data.frame






