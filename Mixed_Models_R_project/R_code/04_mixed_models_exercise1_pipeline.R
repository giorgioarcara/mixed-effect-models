## The present scripts utilizes the same data of the slides and sketches 
# a realistic script for a whole analysis with mixed models.
# This hypothetic experiment has
# two categorical variables:
#
# - StimType: 2 levels (Metaphor vs Literal)
# - PrimeType: 2 levels (Negative vs Positive)
#
# and three covariates
#
# - Imageability
# - Complexity
# - Concreteness
#
# To add some realism. Let's hypotesize that according to some recent results from another study,
# Imageability may play a different role depending on stimulus type (higher for metaphors)


rm(list=ls())
library(lmerTest)
library(effects)
library(car)
library(itsadug)

load("data/example1_dat.RData")

#############
# first make some checks and inspect the dataset
##############
str(dat)
head(dat)
summary(dat)

dat$PrimeType=factor(dat$PrimeType)
dat$StimType=factor(dat$StimType)

# I order the data here to be sure data are ordered
dat = dat[order(dat$Subj_ID, dat$Ntrial), ]


# a check that all subjects have all conditions
table(dat$Subj_ID, dat$StimType, dat$PrimeType)

##################################
# PRELIMINARY CHECKS ON ASSUMPTIONS
##################################
# check data distribution
library(languageR)
pairscor.fnc(dat[, c("Imageability", "Concreteness", "Complexity")])
# there ia very high correlation between Imageability and Concreteness, this will lead probably to issues with Collinearity.
# To be conservative, I will start with random slopes for the categorical variables. But I don't expect too many complex
#

dat.lmer0 = lmer(RT~StimType*PrimeType + Imageability + Concreteness + Complexity + (1+StimType*PrimeType|Subj_ID) + (1|Item_ID), data=dat)

vif(dat.lmer0)
#  multicollinearity (as expected) is an issue here as vif > 10 for some variables. We decide to drop concretness and use only Imageability. 
# we will consider this in the interpretation of the results.
# moreveor there were some issues in model convergence, maybe the random structure is too complex.
 d# Now fit only a model with Imageability, contrasting two possible random structure
 
dat.lmer1 = lmer(RT~StimType*PrimeType + Imageability + Complexity + (1+StimType*PrimeType|Subj_ID) + (1|Item_ID), data=dat)
dat.lmer2 = lmer(RT~StimType*PrimeType + Imageability + Complexity + (1+StimType|Subj_ID) + (1|Item_ID), data=dat)
anova(dat.lmer1, dat.lmer2)
# the anova test suggest that the more complex random structure is better.
#
# finally I check whether a model including an interaction with Imageability
dat.lmer3 = lmer(RT~StimType*PrimeType + StimType*Imageability + Complexity + (1+StimType*PrimeType|Subj_ID) + (1|Item_ID), data=dat)
Anova(dat.lmer3, type="III")
anova(dat.lmer1, dat.lmer3) # I can use Anova cause they are nested
# The anova is not significant so I will keep the simpler model: there is no evidence of relevant interaction between StimType and PrimeType

# now it's time to check everything is good with diagnostic
# 1) normality of residuals
par(mfrow=c(1,2))
hist(resid(dat.lmer1))
qqnorm(resid(dat.lmer1))
qqline(resid(dat.lmer1))
# residuals are good

# 2) check homoscedasticity
par(mfrow=c(1,1))
plot(fitted(dat.lmer1), resid(dat.lmer1))
abline(h=0, col="red", lty=2)
# no  issues here

# 3) check linearity from partial residuals
plot(allEffects(dat.lmer1, partial.residuals=T, cex=0.05))
#no signs of missed non linearities

# 4) independence of residuals
# note that this assumes trials are in the correct order
# (note that I ordered before according to Ntrial, to be sure)

# The figure is very large so I generate it externally
png("Figures/acf_resid_ex1.png", height=400*5, width=400*4, res=180)
Subjects = unique(dat$Subj_ID)

par(mfrow = c(5, 4), mar=c(2,2,3,1))
for (iS in Subjects){
  acf(resid(dat.lmer1)[dat$Subj_ID==iS], main=paste("Subject = ", iS, sep=""))
}

dev.off()
# in no case there are problems in autocorrelation



########
# as the model seems solid, I can make some additional steps, like post hocs or figures
####

Anova(dat.lmer1) # to inspect significance of terms
pairs(emmeans(dat.lmer1, ~StimType*PrimeType)) # for post-hocs
# As there is no interaction with linear terms there is no need to post-hoc for marginal effects.

# here a plot of all marginal effects
plot(allEffects(dat.lmer1))


