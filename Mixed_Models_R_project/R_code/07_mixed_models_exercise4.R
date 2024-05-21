## The present scripts utilizes the same data of the slides and sketches 
# a realistic script for a whole analysis with mixed models and ordinal data.
# This hypothetic experiment has
# two categorical variables:
#
# - StimType: 2 levels (Irony vs Literal)
# - Group: 3 levels (Autism vs Autism Relatives vs Controls)
#
# and three covariates
#
# - TOM: an hypothetical test on Theory of Mind
# - Length: hypothetical length of stimuli (e.g, words or image..)
# - Age: the age of the kid
# - Ntrial: the trial order in the experiment.
#
# We expect worse performance in Autism and Autism relatives.



rm(list=ls())
library(ordinal)
library(effects)
library(car)

load("data/example4_dat.RData")

head(dat_ord)
summary(dat_ord)


################################
# CASE 1 Cumulative Link Models
################################

mod_ord1 <- clmm(rating ~ StimType*Group + TOM + Age + (1|Subj_ID) + (1|Item_ID), data = dat_ord, Hess=TRUE)	
summary(mod_ord1)	


# lines below show some not available feature thata are crucial
#anova(mod_ord1) # not working
#Anova(mod_ord1, type="III") 

# neither predict or residuals are available
#predict(mod_ord1)
# residuals(mod_ord1)

# for model selection you could use AIC
AIC(mod_ord1)

mod_ord2 <- clmm(rating ~ StimType*Group + TOM + Age + (1+StimType|Subj_ID) + (1|Item_ID), data = dat_ord, Hess=TRUE)	
AIC(mod_ord1)	
AIC(mod_ord2)	
# choose the model with lowest AIC


# from https://groups.google.com/g/medstats/c/y_94cReelQg

## the following code is a code to check fulfillment of the "parallel lines" assumption.

mod_bin1 =  glmer(rating>=2 ~ StimType*Group + TOM + Age + (1|Subj_ID) + (1|Item_ID), data = dat_ord, family="binomial")

y <- as.factor(dat_ord$rating)
Y <- as.numeric(y) - 1
ncut <- length(unique(Y)) - 1
cuts = 2:5
p <- dim(summary(mod_bin1)$coef)[1]-1 # length of coefficients (except Intercept)
Coef <- matrix(NA, ncol=p, nrow=ncut,
               dimnames=list(paste('>=', levels(y)[-1],sep=''),
                             NULL))
for(k in 1:ncut) {
  curr_cut = cuts[k]
  f <- glmer(rating>=curr_cut ~ StimType*Group + TOM + Age + (1|Subj_ID) + (1|Item_ID), data = dat_ord, family="binomial")
  Coef[k,] <- t(summary(f)$coef[-1, 1])
}
colnames(Coef) <- names(summary(f)$coef[-1, 1])
round(Coef, 3)

curr_Coef=4
plot(-1:1, -1:1, type="n", xlab="X", ylab="Y", main=paste("Parallel Lines\n", colnames(Coef)[curr_Coef], sep=""))
for (iC in 1:dim(Coef)[1]){
  abline(0, b=Coef[iC, curr_Coef], lty=iC)
}
legend("topright", legend = rownames(Coef), col = 1:dim(Coef)[1], lty = 1:dim(Coef)[1])
## Note that these plots may be interpreted only qualitatevly (there are no thresholds.)


####################################
# CASE 2 Generalized Additive Models
####################################
# an alternative is using gam with "ocat" (ordinal category) family
library(mgcv)
dat_ord$rating_num = as.numeric(dat_ord$rating) # gam with "ocat" family works only with numeric

# for info on syntax see https://jacolienvanrij.com/Tutorials/GAMM.html#random-effects
mod_gam = gam(rating_num~StimType*Group + Length + TOM + Age + s(Subj_ID, bs="re") + s(Item_ID, bs="re"), data = dat_ord, family=ocat(R=5))
summary(mod_gam)

##
y <- as.factor(dat_ord$rating)
Y <- as.numeric(y) - 1
ncut <- length(unique(Y)) - 1
cuts = 2:5

# start with an initial model
mod_bin1 <- gam(rating>=2~StimType*Group + TOM + Age + s(Subj_ID, bs="re") + s(Item_ID, bs="re"), data = dat_ord, family="binomial")


p <- length(summary(mod_bin1)$p.coef)-1 # length of coefficients (except Intercept)
Coef <- matrix(NA, ncol=p, nrow=ncut,
               dimnames=list(paste('>=', levels(y)[-1],sep=''),
                             NULL))
for(k in 1:ncut) {
  curr_cut = cuts[k]
  f <- gam(rating>=curr_cut~StimType*Group + TOM + Age + s(Subj_ID, bs="re") + s(Item_ID, bs="re"), data = dat_ord, family="binomial")
  Coef[k,] <- t(summary(f)$p.table[-1, 1])
}
colnames(Coef) <- names(summary(f)$p.table[-1, 1])
round(Coef, 3)

curr_Coef=4
plot(-1:1, -1:1, type="n", xlab="X", ylab="Y", main=paste("Parallel Lines\n", colnames(Coef)[curr_Coef], sep=""))
for (iC in 1:dim(Coef)[1]){
  abline(0, b=Coef[iC, curr_Coef], lty=iC)
}
legend("topright", legend = rownames(Coef), col = 1:dim(Coef)[1], lty = 1:dim(Coef)[1])
## Note that these plots may be interpreted only qualitatevly (there are no thresholds.)

gam.check(mod_gam)

library(DHARMa)
simOut = simulateResiduals(fittedModel = mod_gam)
plot(simOut)

plotResiduals(simOut, dat_ord$TOM)
plotResiduals(simOut, dat_ord$Length)
plotResiduals(simOut, dat_ord$Age)
plotResiduals(simOut, dat_ord$StimType)
plotResiduals(simOut, dat_ord$Group)
plotResiduals(simOut, dat_ord$Subj_ID)
plotResiduals(simOut, dat_ord$Item_ID)









