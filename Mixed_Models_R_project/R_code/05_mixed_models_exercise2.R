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
load("data/example2_dat.RData")
# now check the other scripts and perform your mixed model analysis!

library(lme4)
library(lmerTest)
library(car)
library(emmeans)

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


mod1 = lmer(RT~StimType*Difficulty + Interpretability + Concreteness + NumberOfLetters + (1 + StimType*Difficulty | Subj_ID) + (1 | Item_ID), data=dat)
mod2 = lmer(RT~StimType*Difficulty + Interpretability + Concreteness + NumberOfLetters + (0+Interpretability | Subj_ID) + (0+Concreteness| Subj_ID) + (0+NumberOfLetters|Subj_ID) + (1 + StimType*Difficulty | Subj_ID) + (1 | Item_ID), data=dat)

vif(mod1)
vif(mod2)

anova(mod1, mod2)

mod3 = lmer(RT~StimType*Difficulty + StimType*Interpretability + Concreteness + NumberOfLetters + (1 + StimType * Difficulty | Subj_ID) +  (1 + StimType | Item_ID), data=dat)

Anova(mod3)
AIC(mod1)
AIC(mod3)


# post-hoc
plot(effect("StimType*Difficulty", mod3))
pairs(emmeans(mod3, ~StimType*Difficulty))

plot(effect("StimType*Interpretability", mod3))
pairs(emtrends(mod3, ~StimType, var="Interpretability"))


# in the end, don't forget to check the assumptions! (see also other examples)



