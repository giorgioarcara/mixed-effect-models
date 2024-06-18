## The second exercise is again on a hypothetical experiment
# Let's give some context:
# It is an experiment that record Reaction Times in a task with two main variables.
# 
# - StimType: 3 levels (Idiom, Metaphor, Proverb)
# - Difficulty: 2 levels (Easy vs Hard)
#
# and three covariates
#
# - Interpretability
# - Concreteness
# - NumberOfLetters
#
# 
# There is some concern that Interpretability and Concreteness may be highly correlated (issues of collinearity?)
# Moroever according to some recent papers effect of Interpretability may vary across the different stimulus type.
# There could also be some effect of fatigue during the experiment, that could be taken into account using Ntrial as a covariate
#
load("data/example5_dat.RData")
# now check the other scripts and perform your mixed model analysis!

library(lme4)
library(lmerTest)
library(car)
library(effects)
library(performance)
library(DHARMa)

str(dat)
head(dat)
tail(dat)

# fix problems.
dat$Subj_ID=factor(dat$Subj_ID)
dat$Item_ID = factor(dat$Item_ID)
dat$Difficulty=factor(dat$Difficulty)
dat$StimType = factor(dat$StimType)

str(dat)
summary(dat)

library(languageR)
pairscor.fnc(dat[, c("Concreteness", "Interpretability", "NumberOfLetters", "Ntrial"), ])
table(dat$Subj_ID, dat$StimType, dat$Difficulty)

hist(dat$RT)

dat$log_RT = log(dat$RT)


mod1 = lmer(RT~StimType*Difficulty + Interpretability + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)
mod2 = lmer(RT~StimType*Difficulty + Interpretability + Concreteness + NumberOfLetters + (1 | Subj_ID)  + (1 | Item_ID), data=dat)

vif(mod1)
vif(mod2)

anova(mod1, mod2)

AIC(mod1)
AIC(mod2)

plot(allEffects(mod2, partial.residuals=T))
# in the end, don't forget to check the assumptions
plot(check_normality(mod1, type="qq"))

plot(fitted(mod2), resid(mod2))

# fit non linearity

# with I() function
mod1a = lmer(RT~StimType*Difficulty + Interpretability + I(Interpretability^2) + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)

plot(allEffects(mod1a, partial.residuals=T))

# with poly()
mod1b = lmer(RT~StimType*Difficulty + poly(Interpretability,2) + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)

plot(allEffects(mod1b, partial.residuals=T))


# with rcs()
library(rms)
mod1c = lmer(RT~StimType*Difficulty + rcs(Interpretability) + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)

plot(allEffects(mod1c, partial.residuals=T))

summary(mod1c)


## FIT GAM MODEL ###


library(mgcv)

mod3 = gam(RT~StimType*Difficulty + s(Interpretability) + s(Concreteness) + NumberOfLetters + s(Item_ID, bs="re") + s(Subj_ID, bs="re"), data=dat)
summary(mod3)

AIC(mod3)
AIC(mod1)

# note the 
library(DHARMa)
simOut3 = simulateResiduals(mod3)
plot(simOut3)
plotResiduals(simOut3, dat$Concreteness)
plotResiduals(simOut3, dat$StimType)
plotResiduals(simOut3, dat$Difficulty)
plotResiduals(simOut3, dat$NumberOfLetters)


library(DHARMa)
simOut1 = simulateResiduals(mod1)
plot(simOut1)
plotResiduals(simOut1, dat$Concreteness)
plotResiduals(simOut1, dat$Interpretability)
plotResiduals(simOut1, dat$StimType)
plotResiduals(simOut1, dat$Difficulty)


