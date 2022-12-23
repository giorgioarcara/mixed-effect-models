## The second exercise is again on a hypothetical experiment
# Let's give some context:
# It is an experiment that record Accuracy in a task with two main variables.
# 
# - StimType: 2 levels (Implicatures, Explicatures)
# - Group: 3 levels (Controls, Schizophrenics, Alzheimer)
#
# and three covariates
#
# - Interpretability (Item level)
# - Age (Subj level)
# - TOM (Subj level)
# - Ntrial (number of trial, as potentially influent nuisance variable)


# you have 5 items per each StimType
# and 10 Subjects per group


#
# In the past there have been some concerns that results are related to unbalanced Age across groups.
# Additionally some people claim that there could be effect of TOM (possibly different across StimType), but be careful
# performance in TOM task may be highly collinear with age.
# 
# There are recent results on this interpretability variable that may play a role not clear.
rm(list=ls())


load("data/example3bin_dat.RData")

# now check the other scripts and perform your mixed model analysis!

library(lme4)
library(lmerTest)
library(car)
library(languageR)
library(performance)


str(dat_bin)
head(dat_bin)
tail(dat_bin)

pairscor.fnc(dat_bin[, c("Age", "TOM"), ])

table(dat_bin$Subj_ID, dat_bin$Group)
table(dat_bin$Item_ID, dat_bin$StimType)

# in perforfming analysis I will use the approach described in my slides (see p. 110 and 11 of my slides updated at 23/12/2022)
# step 1) check issues on random part
# step 2) check issues on convergence
# step 3) check issues on model assumptions
# only models that pass all the three steps are included in the model selection.

# by following hypothesis include only a random slope of Stimulus Type on Subjects 
# (as it is a property related to Items) to Subject, 
mod1a = glmer(ACC~Group*StimType + Age + TOM + Interpretability + Ntrial + (1 + StimType | Subj_ID) + (1  | Item_ID), data=dat_bin, family="binomial")
summary(mod1a)
# as first check I inspect correlation of random slope. For StimType, there is a correlation = 1. I try to force uncorrelated random slope and intercept
mod1b = glmer(ACC~Group*StimType + Age + TOM + Interpretability + Ntrial + (0 + StimType | Subj_ID) + (1  | Item_ID), data=dat_bin, family="binomial")
summary(mod1b)
#  as for model a, also model b show problems with the random structure (1 correlation), suggesting overparametrization of random part
mod1c = glmer(ACC~Group*StimType + Age + TOM + Interpretability + Ntrial + (1 | Subj_ID) + (1  | Item_ID), data=dat_bin, family="binomial")
summary(mod1c)

## mod1c show some problems with convergence, that I explore with allFit
mod1c_af = allFit(mod1c)
summary(mod1c_af)
# results are very similar across Optimizers, so results are good enough

# the final need check is on assumptions (see p. 110 and 11 of my slides updated at 23/12/2022)
check_model(mod1c)
binned_residuals(mod1c)
# both checks shows a very reasonable fulfillment of assumptions

# residuals are good. I use Anova for checking significant terms
Anova(mod1c, type="III")

plot(allEffects(mod1c))




