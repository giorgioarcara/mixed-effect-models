rm(list=ls())
library(lmerTest)
library(faux)
library(effects)
set.seed(100)

##########################
# SIMULATE MIXED MODEL DATA
#######################################
# Author: Giorgio Arcara
# date: 07/2021
# This script contain some code to simulate data for mixed model
# I strongly based the code by the simulation by Lisa de Bruine (https://debruine.github.io/tutorials/sim-lmer.html),
# and by some lessons by Harald Baayen. Blame on my for all the mistakes.


#########################
# FIXED EFFECTS #########
#########################
# define population level fixed effect, for factor (use just one factor for all)
# and for covariates

# here for example I have two factors F1 and F2 with two levels each 
# so I put all the combinations in just one factor.

Cond_labels = c("Control_Irony", "Autism_Irony", "AutRelative_Irony", "Control_Literal", "Autism_Literal", "AutRelative_Literal")

# here the effect at population levels
Cond_effs = c(+80, +10, +50, +10, +05, +10)

Cond_adj = data.frame(Cond_labels, Cond_effs)

# other relevant value to define final formula
Grand_Intercept = 2000

# sigma
mod_sigma = 10

Dep_name = "latent_var"
# it is used here just to generate the data


#########################
# PART1 - SUBJECTS ######
#########################
# Define parameters of subjects, and subject-specific slopes
Subj_n = 50 # number of subjects

Subj_ID = 1:Subj_n

Subj_sd = 100 # the Subject random intercept (NOTE: it is an sd)

## add random slope here with no correlation with intercept
Subj_effsd = c(100, 100, 80, 100, 100) # note it is an sd
vars_cor = c(0.4) # correlations for RIntercept and Rslope
vars_n = 1+length(Subj_effsd) # Rand Intercept + Rand Slopes
Subj_adj = rnorm_multi(n=Subj_n, mu=0, r =vars_cor, 
                        sd = c(Subj_sd, Subj_effsd), varnames = c("Subj_rInt", Cond_labels[-1]))

# check that it worked
cor(Subj_adj)
hist(Subj_adj$Autism_Irony)


Subj_adj = cbind(Subj_ID, Subj_adj)
# note that I start from 3, which is the 
Subj_Cond_adj = reshape(Subj_adj, direction="long", idvar="Subj_ID", varying=list(3:(length(Subj_adj))), times = names(Subj_adj)[3:length(Subj_adj)])
names(Subj_Cond_adj) = c("Subj_ID", "Subj_rInt_adj", "Condition", "SubjCond_adj")
row.names(Subj_Cond_adj)=1:dim(Subj_Cond_adj)[1]



# now divide Subject Intercept from Subj random slope
#xyplot(SubjCond_adj~Subj_rInt_adj|Condition, Subj_Cond_adj)


Subj_Cond_adj$Subj_rInt_adj = NULL # delete the column with Random Intercept, to avoid confusion


## add some variables to items (not interacting with anything)
Subj_adj$Age = rnorm(Subj_n, mean = 10, sd = 5)
Subj_adj$Age_adj = Subj_adj$Age * (-0.2) + Subj_adj$Age^2 * (-5)

Subj_adj$TOM = rnorm(Subj_n, mean = 30,  sd = 1.5)
Subj_adj$TOM_adj = Subj_adj$TOM * -6


# get random intercept here
Subj_rInt_adj = Subj_adj[, c("Subj_ID", "Subj_rInt", "Age", "Age_adj", "TOM", "TOM_adj")]  


##########################
# PART2 - ITEMS  ########
#########################
# Define parameters of Items, currently without Item-specific slopes
Item_n = 10 # number of Total Items.
Item_sd = 20
Item_ID = 1:Item_n
Item_adj =  data.frame(Item_ID , 
                            Item_rInt = rnorm(Item_n, mean=0, sd=Item_sd)
)


Item_adj$Length = rnorm(Item_n, mean=10, sd = 3)
Item_adj$Length_adj = Item_adj$Length * -0.5




####################################
# PART 3 CREATE DATA.FRAME COMBINATION
####################################
# here you create a combination of Subject, Items, and Condition, to create the "Experiment"
dat_skeleton = expand.grid(Subj_ID = Subj_ID, Item_ID = Item_ID, Condition = Cond_labels)

# here I created all the possible combinations. However, depending from the experiment, this could make not sense.
# for example, I want Item from 1:100 are associated to F11, while Item from 101:200 to F12
# all Items are associated to F2.
# to do this, I remove the non relevant combination here (sorry, some pretty bad hard coding here).
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(1:5) & dat_skeleton$Condition%in%c("Control_Irony", "Autism_Irony", "AutRelative_Irony")), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(6:10) & dat_skeleton$Condition%in%c("Control_Literal", "Autism_Literal", "AutRelative_Literal")), ]

dat_skeleton  = dat_skeleton[!(dat_skeleton$Subj_ID%in%c(1:10) & dat_skeleton$Condition%in%c("Autism_Irony", "AutRelative_Irony","Autism_Literal", "AutRelative_Literal" )), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Subj_ID%in%c(11:20) & dat_skeleton$Condition%in%c("Control_Irony", "AutRelative_Irony","Control_Literal", "AutRelative_Literal" )), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Subj_ID%in%c(21:30) & dat_skeleton$Condition%in%c("Control_Irony", "Autism_Irony","Control_Literal", "Autism_Literal" )), ]


table(dat_skeleton$Item_ID, dat_skeleton$Subj_ID)
table(dat_skeleton$Condition, dat_skeleton$Subj_ID)

# add condition effects
dat_skeleton = merge(dat_skeleton, Cond_adj, by.x=c("Condition"), by.y="Cond_labels")

# add by-Subject random intercept
dat_skeleton = merge(dat_skeleton, Subj_rInt_adj, by="Subj_ID")

# add by-subj random slopes
dat_skeleton = merge(dat_skeleton, Subj_Cond_adj, by=c("Subj_ID", "Condition"), all.x=T)
dat_skeleton[is.na(dat_skeleton$SubjCond_adj), "SubjCond_adj"] = 0 
# above I add an adjustment of 0 of the level in the data that was in the Intercept

# add by-Item random Intercept
dat_skeleton = merge(dat_skeleton, Item_adj, by=c("Item_ID"))

# add Grand Intercept
dat_skeleton$Grand_Intercept = Grand_Intercept

# add error
dat_skeleton$Error = rnorm(dim(dat_skeleton)[1], mean=0, sd=mod_sigma)

### simulate nTrial for all subjects
dat_skeleton$Ntrial=NA
Subjects = unique(dat_skeleton$Subj_ID)
for (iS in Subjects){
  tot_ntrials = dim(dat_skeleton[dat_skeleton$Subj_ID==iS,])[1]
  dat_skeleton[dat_skeleton$Subj_ID==iS, "Ntrial"] = sample(1:tot_ntrials, tot_ntrials, replace=F)
  
}

dat_skeleton$Ntrial_adj = 0.0005*dat_skeleton$Ntrial




# ADD DEPENDENT VARIABLE (combining everything)
dat_skeleton[, Dep_name] = apply(dat_skeleton[, c("Grand_Intercept", "Cond_effs", "Subj_rInt",
                                                  "SubjCond_adj", "Item_rInt", 
                                                  "TOM_adj", "Age_adj", "Length_adj", "Ntrial_adj",
                                                  "Error")], 1, sum)
head(dat_skeleton)

hist(dat_skeleton$latent_var)


plot(dat_skeleton$Length, dat_skeleton$latent_var)

mod1 = lmer(latent_var~Condition+Length+TOM+Age+Ntrial+(1|Subj_ID) + (1|Item_ID), data=dat_skeleton)
plot(effect("Length", mod1, partial.residuals=T))

library(car)
vif(mod1)
plot(allEffects(mod1))
summary(mod1)

mod2 = lmer(latent_var~Condition+Length+I(Length^2)+TOM+Age+(1|Subj_ID) + (1|Item_ID), data=dat_skeleton)

summary(mod2)
plot(allEffects(mod2))
plot(effect("Condition", mod2))




# check correspondence of estimates with model 2
#plot(Subj_Cond_adj$SubjCond_adj[1:20], ranef(mod2)$Subj_ID$ConditionF11_F22)
#plot(Subj_rInt_adj$Subj_rInt, ranef(mod2)$Subj_ID[["(Intercept)"]])

# createa dataset 
dat_skeleton$Group=NA
dat_skeleton[grep("Control", dat_skeleton$Condition), "Group"]="Control"
dat_skeleton[grep("Autism", dat_skeleton$Condition), "Group"]="Autism"
dat_skeleton[grep("AutRelative", dat_skeleton$Condition), "Group"]="AutRelative"

dat_skeleton$StimType=NA
dat_skeleton[grep("Irony", dat_skeleton$Condition), "StimType"]="Irony"
dat_skeleton[grep("Literal", dat_skeleton$Condition), "StimType"]="Literal"


table(dat_skeleton$StimType, dat_skeleton$Condition)
table(dat_skeleton$Group, dat_skeleton$Condition)

dat = dat_skeleton[, c("Subj_ID", "Item_ID", "TOM", "Length", "Age", "Group", "StimType", "Ntrial", "latent_var")]
dat$Subj_ID=factor(dat$Subj_ID)
dat$Item_ID=factor(dat$Item_ID)
dat$StimType = factor(dat$StimType)
dat$Group = factor(dat$Group)


mod2 = lmer(latent_var~StimType*Group+ StimType*Length+TOM+Age+(1+StimType*Group|Subj_ID) + (1|Item_ID), data=dat)
summary(mod2)

plot(allEffects(mod2))
plot(effect(mod2, term="StimType*Length"))


dat_ord = dat
dat_ord$rating = NA
curr_size = dim(dat_ord)[1]/5 # cause I'm determining for levels of latent variable
dat_ord$rating[dat_ord$latent_var > quantile(dat_ord$latent_var, prob=0.8)] = sample(1:5, size =  curr_size, prob=c(0,0.1, 0.1, 0.3, 0.6), replace=T)
dat_ord$rating[dat_ord$latent_var <= quantile(dat_ord$latent_var, prob=0.8)] = sample(1:5, size =  curr_size, prob=c(0.05,0.05, 0.2, 0.5, 0.2), replace=T)
dat_ord$rating[dat_ord$latent_var <= quantile(dat_ord$latent_var, prob=0.6)] = sample(1:5, size =  curr_size, prob=c(0.1,0.1, 0.5, 0.1, 0.1), replace=T)
dat_ord$rating[dat_ord$latent_var <= quantile(dat_ord$latent_var, prob=0.4)] = sample(1:5, size =  curr_size, prob=c(0.2,0.5, 0.2, 0.05, 0.05), replace=T)
dat_ord$rating[dat_ord$latent_var <= quantile(dat_ord$latent_var, prob=0.2)] = sample(1:5, size =  curr_size, prob=c(0.5,0.3, 0.1, 0.05, 0.05), replace=T)

table(dat_ord$rating)
dat_ord$latent_var = NULL
dat_ord$ACC = NULL
dat_ord$rating = ordered(dat_ord$rating) # for analysis with ordinal

save(dat_ord, file="data/example4_dat.RData")


library(ordinal)
mod_ord = clmm(rating~StimType*Group+ Length+TOM+Age+(1|Subj_ID) + (1|Item_ID), data=dat_ord, link="probit")
summary(mod_ord)

mod_ord = clmm2(rating~StimType,  random = Subj_ID,   nominal = ~Length, data=dat_ord, Hess=TRUE)
summary(mod_ord)

library(mgcv)
dat_skeleton$Item_ID=factor(dat_skeleton$Item_ID)
dat_skeleton$Subj_ID=factor(dat_skeleton$Subj_ID)

dat_ord$rating_num = as.numeric(dat_ord$rating)
mod_ord2 = gam(rating_num~StimType*Group + TOM + s(Age) + Length + Ntrial + s(Subj_ID, bs="re") + s(Item_ID, bs="re") , family=ocat(R=5), data=dat_ord)
summary(mod_ord2)


mod_ord_s = gam(latent_var~StimType*Group + TOM + s(Age) + Length + Ntrial + s(Subj_ID, bs="re") + s(Item_ID, bs="re") , data=dat_skeleton)

plot(mod_ord_s)

library(gratia)
p1 <- draw(mod_ord_s, select = "TOM")


?anova(mod_ord2)

#plot(mod_ord2, all.terms=TRUE)

library(DHARMa)
simOut = simulateResiduals(fittedModel = mod_ord_s)
plot(simOut)
plotResiduals(simOut, dat$TOM)
plotResiduals(simOut, dat$Length)
plotResiduals(simOut, dat$Age)
plotResiduals(simOut, dat$StimType)
plotResiduals(simOut, dat$Group)
plotResiduals(simOut, dat$Ntrial)
plotResiduals(simOut, dat$Subj_ID)
plotResiduals(simOut, dat$Item_ID)



plot(mod_ord2, resid=T)


plot(mod_ord2, select=1)  # Plot the first smooth term
gam.check(mod_ord2)

library(itsadug)

check_resid(mod_ord_s)

# https://cran.r-project.org/web/packages/gratia/vignettes/custom-plotting.html

