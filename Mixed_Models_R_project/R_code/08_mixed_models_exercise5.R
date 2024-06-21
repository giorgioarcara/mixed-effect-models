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

# model with random slope
mod1 = lmer(RT~StimType*Difficulty + Interpretability + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)

# check assumptions
vif(mod1)
plot(check_normality(mod1))
plot(check_normality(mod1, type="qq"))
# partial residuals (fundamental for nonlinearities)
plot(allEffects(mod2, partial.residuals=T))

# a non linearity seems present, check several ways to fit it.

# with I() function
mod1a = lmer(RT~StimType*Difficulty + Interpretability + I(Interpretability^2) + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)
summary(mod1a)
plot(check_normality(mod1a))
plot(check_normality(mod1a, type="qq"))
# partial residuals (fundamental for nonlinearities)
plot(allEffects(mod1a, partial.residuals=T))

# for simplicity in the next checks I don't include also diagnostic checks.

# check with power of 3
mod1a2 = lmer(RT~StimType*Difficulty + Interpretability + I(Interpretability^2) + I(Interpretability^3) + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)

# with poly()
mod1b = lmer(RT~StimType*Difficulty + poly(Interpretability,2) + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)
summary(mod1b)

plot(allEffects(mod1b, partial.residuals=T))

# with rcs()
library(rms) # rcs() is inside rms package
mod1c = lmer(RT~StimType*Difficulty + rcs(Interpretability) + Concreteness + NumberOfLetters + (1 + StimType | Subj_ID) + (1 | Item_ID), data=dat)

plot(allEffects(mod1c, partial.residuals=T))

# compare AIC of all models
AIC(mod1, mod1a, mod1a2, mod1b, mod1c)

# mod1c seems the best (using AIC)

## FIT GAM MODEL ###

library(mgcv)

# model with smooth term for Interpretability, smooth term for Concreteness and two random smooth for Item and Subject
mod2 = gam(RT ~ StimType*Difficulty + s(Interpretability) + s(Concreteness) + NumberOfLetters + s(Item_ID, bs="re") + s(Subj_ID, StimType, bs="re"), data=dat)
summary(mod2)

AIC(mod2)
AIC(mod1)

plot(mod2)

## check if simpler model are more adequate using a model comparison approach
# remove random smooth for concreteness
mod2a = gam(RT ~ StimType*Difficulty + s(Interpretability) + Concreteness + NumberOfLetters + s(Item_ID, bs="re") + s(Subj_ID, StimType, bs="re"), data=dat)


AIC(mod2, mod2a)
# note they are identical cause GAM guessed a linear effect of Concretness (edf = 1)

AIC(mod2, mod1)

# be careful, don't make your choice only in AIC, also check diagnostics (in this case partial residuals of effects)
summary(mod2)
plot(mod2)

# diagnostics
par(mfrow(c(2,2)))
gam.check(mod2)

# 
library(itsadug)
plot_smooth(mod2, view = "Interpretability")

# to have some hints about the need of adding non linear terms, you can plot raw data (dependent variable againts predictor)
plot(dat$Interpretability, dat$RT)
plot(dat$Concreteness, dat$RT)


library(DHARMa)
simOut2 = simulateResiduals(mod2)
plot(simOut2)
plotResiduals(simOut2, dat$Concreteness)
plotResiduals(simOut2, dat$Interpretability)
plotResiduals(simOut2, dat$StimType)
plotResiduals(simOut2, dat$Difficulty)


