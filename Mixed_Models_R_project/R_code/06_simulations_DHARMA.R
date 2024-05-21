rm(list=ls())

library(DHARMa)
library(lme4)
library(effects)
library(performance)

# Create Data is a useful function to create syntetic data with known problems
set.seed(100)
testData1 = createData(sampleSize = 200,  family = poisson())

# by default we have a linear varsiable (Environment) a random grouping variable (Group)-
# so you should interpret group as the number of participants.


head(testData1)
hist(testData1$observedResponse)

fittedModel1 <- glm(observedResponse ~  Environment1 , family = "poisson", data = testData1)

simulationOutput1 <- simulateResiduals(fittedModel = fittedModel1)
plot(simulationOutput1)

testDispersion(simulationOutput1) # note that the
testZeroInflation(simulationOutput1)
# note that here we find both Overdispersion and ZeroInflation, but the reality is that we are missing some
# relevant predictors!
png("Figures/DHARMa_plot.png", res=150, width=1600, height=800)
plot(simulationOutput1)
dev.off()


fittedModel1a <- glmer(observedResponse ~  Environment1   + (1|group), family = "poisson", data = testData1)
simulationOutput1a <- simulateResiduals(fittedModel = fittedModel1a)
plot(simulationOutput1a)
testOverdispersion(simulationOutput1a)
testZeroInflation(simulationOutput1a)
# a more appropriate fit cancels all the issues.

set.seed(100)
testData2 = createData(sampleSize = 200, overdispersion = 2, family = poisson())
fittedModel2 <- glmer(observedResponse ~  Environment1 + (1|group), family = "poisson", data = testData2)
hist(testData2$observedResponse)

simulationOutput2 <- simulateResiduals(fittedModel = fittedModel2)
plot(simulationOutput2)
testOverdispersion(simulationOutput2)
testZeroInflation(simulationOutput2)

## fit a negative binomial model with GLMMadaptive
# https://drizopoulos.github.io/GLMMadaptive/

library(GLMMadaptive)

fittedModel2_zi = nb_model <- mixed_model(fixed = observedResponse ~ Environment1,
                                          random = ~ 1 | group,
                                          data = testData2,
                                          family = negative.binomial())


simulationOutput2 <- simulateResiduals(fittedModel = fittedModel2_zi)
plot(simulationOutput2)
testOverdispersion(simulationOutput2)
testZeroInflation(simulationOutput2)

summary(fittedModel2_zi)

eff_plot = effectPlotData(fittedModel2_zi, 
                          newdata=data.frame(Environment1=seq(min(testData2$Environment1), 
                                                              max(testData2$Environment1), length.out=10)))

library(lattice)
xyplot(pred + low + upp ~ Environment1, data = eff_plot,
       type = "l", lty = c(1, 2, 2), col = c(2, 1, 1), lwd = 2,
       xlab = "Environment1", ylab = "y")



#install.packages("glmTMB")
# not working
#https://github.com/glmmTMB/glmmTMB/issues/883

#library(glmmTMB)
#fittedModel <- glmmTMB(observedResponse ~ Environment1  + (1|group), ziformula = ~1 , family = "poisson", data = testData2)
#summary(fittedModel)
