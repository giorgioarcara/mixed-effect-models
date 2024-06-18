rm(list=ls())
library(effects)
library(performance)
set.seed(123)
n = 1000
x = rnorm(n, mean=0, sd=20)
err = rnorm(n, sd = 10)
Intercept = 10
y = Intercept + 0.08*(x^2) - 0.8*(x) + err

#plot(x,y)

mod = lm(y~x)

summary(mod)

plot(x,y)
plot(allEffects(mod))

hist(resid(mod))

plot(check_normality(mod), type="qq")
plot(check_normality(mod))
hist(scale(residuals(mod)))
plot(check_heteroscedasticity(mod))

plot(allEffects(mod, partial.residuals=TRUE))

png("Figures/partial_residuals.png", height=800, width=800, res=150)
plot(allEffects(mod, partial.residuals=TRUE))
dev.off()


simulationOutput <- simulateResiduals(fittedModel = mod)
plot(simulationOutput)
testOverdispersion(simulationOutput)
testZeroInflation(simulationOutput)



# add non-linear tems

mod2 = lm(y~x+I(x^2))

summary(mod2)

plot(check_normality(mod2), type="qq")
plot(check_normality(mod2))
hist(scale(residuals(mod2)))
plot(check_heteroscedasticity(mod2))

plot(allEffects(mod2, partial.residuals=TRUE))

#
n = 1000
x = rnorm(n, mean=0, sd=20)
err = rnorm(n, sd = 2)
Intercept = 10
y = Intercept + 0.08*x + err

plot(x,y)

mod3 = lm(y~x)

plot(allEffects(mod3, partial.residuals=T))
plot(allEffects(mod3))
resid(mod3)

hist(residuals(mod3))
plot(check_normality(mod3), type="qq")
plot(check_normality(mod3))
hist(scale(residuals(mod3)))
plot(check_heteroscedasticity(mod3))


