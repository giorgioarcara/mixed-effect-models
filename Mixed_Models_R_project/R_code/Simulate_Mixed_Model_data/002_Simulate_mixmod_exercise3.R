rm(list=ls())
library(lmerTest)
library(faux)
library(effects)
set.seed(140)

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

Cond_labels = c("Controls_Explicature", "Schizophrenics_Explicature", "Alzheimer_Explicature", "Controls_Implicature", "Schizophrenics_Implicature", "Alzheimer_Implicature")

# here the effect at population levels
Cond_effs = c(0, +10, +20, +40, +80, +180)

Cond_adj = data.frame(Cond_labels, Cond_effs)

# other relevant value to define final formula
Grand_Intercept = 1700

# sigma
mod_sigma = 40

Dep_name = "RT"



#########################
# PART1 - SUBJECTS ######
#########################
# Define parameters of subjects, and subject-specific slopes
Subj_n = 30 # number of subjects

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
hist(Subj_adj$Schizophrenics_Explicature)


Subj_adj = cbind(Subj_ID, Subj_adj)
# note that I start from 3, which is the 
Subj_Cond_adj = reshape(Subj_adj, direction="long", idvar="Subj_ID", varying=list(3:(length(Subj_adj))), times = names(Subj_adj)[3:length(Subj_adj)])
names(Subj_Cond_adj) = c("Subj_ID", "Subj_rInt_adj", "Condition", "SubjCond_adj")
row.names(Subj_Cond_adj)=1:dim(Subj_Cond_adj)[1]



# now divide Subject Intercept from Subj random slope
#xyplot(SubjCond_adj~Subj_rInt_adj|Condition, Subj_Cond_adj)


Subj_Cond_adj$Subj_rInt_adj = NULL # delete the column with Random Intercept, to avoid confusion


## add some variables to items (not interacting with anything)
Subj_adj$Age = rnorm(Subj_n, mean = 60, sd = 10)
Subj_adj$Age_adj = Subj_adj$Age * (-0.5)

Subj_adj$TOM = rnorm(Subj_n, mean = 3, sd = 1.5)
Subj_adj$TOM_adj = Subj_adj$TOM * 30


# get random intercept here
Subj_rInt_adj = Subj_adj[, c("Subj_ID", "Subj_rInt", "Age", "Age_adj", "TOM", "TOM_adj")]  


##########################
# PART2 - ITEMS  ########
#########################
# Define parameters of Items, currently without Item-specific slopes
Item_n = 10 # number of Total Items.
Item_sd = 80
Item_ID = 1:Item_n
Item_adj =  data.frame(Item_ID , 
                            Item_rInt = rnorm(Item_n, mean=0, sd=Item_sd)
)


## simulate two moderate correlated variabls 
Item_adj$Interpretability = rnorm(Item_n, mean=10, sd = 3)
Item_adj$Interpretability_adj = Item_adj$Interpretability* -3
Item_adj$Interpretability_adj[6:10] = Item_adj$Interpretability[6:10]*-60
# note that I choose these items cause few lines below I will define these are items for Proverbs (easy and hard)




####################################
# PART 3 CREATE DATA.FRAME COMBINATION
####################################
# here you create a combination of Subject, Items, and Condition, to create the "Experiment"
dat_skeleton = expand.grid(Subj_ID = Subj_ID, Item_ID = Item_ID, Condition = Cond_labels)

# here I created all the possible combinations. However, depending from the experiment, this could make not sense.
# for example, I want Item from 1:100 are associated to F11, while Item from 101:200 to F12
# all Items are associated to F2.
# to do this, I remove the non relevant combination here (sorry, some pretty bad hard coding here).
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(1:5) & dat_skeleton$Condition%in%c("Controls_Explicature", "Schizophrenics_Explicature", "Alzheimer_Explicature")), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(6:10) & dat_skeleton$Condition%in%c("Controls_Implicature", "Schizophrenics_Implicature", "Alzheimer_Implicature")), ]

dat_skeleton  = dat_skeleton[!(dat_skeleton$Subj_ID%in%c(1:10) & dat_skeleton$Condition%in%c("Schizophrenics_Explicature", "Alzheimer_Explicature","Schizophrenics_Implicature", "Alzheimer_Implicature" )), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Subj_ID%in%c(11:20) & dat_skeleton$Condition%in%c("Controls_Explicature", "Alzheimer_Explicature","Controls_Implicature", "Alzheimer_Implicature" )), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Subj_ID%in%c(21:30) & dat_skeleton$Condition%in%c("Controls_Explicature", "Schizophrenics_Explicature","Controls_Implicature", "Schizophrenics_Implicature" )), ]


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
                                                  "TOM_adj", "Age_adj", "Interpretability_adj", "Ntrial_adj",
                                                  "Error")], 1, sum)
head(dat_skeleton)

hist(dat_skeleton$RT)

mod1 = lmer(RT~Condition+Interpretability+TOM+Age+Ntrial+(1|Subj_ID) + (1|Item_ID), data=dat_skeleton)
library(car)
vif(mod1)
plot(allEffects(mod1))
summary(mod1)

mod2 = lmer(RT~Condition*Interpretability+TOM+Age+(1|Subj_ID) + (1|Item_ID), data=dat_skeleton)

summary(mod2)



# check correspondence of estimates with model 2
#plot(Subj_Cond_adj$SubjCond_adj[1:20], ranef(mod2)$Subj_ID$ConditionF11_F22)
#plot(Subj_rInt_adj$Subj_rInt, ranef(mod2)$Subj_ID[["(Intercept)"]])

# createa dataset 
dat_skeleton$Group=NA
dat_skeleton[grep("Controls", dat_skeleton$Condition), "Group"]="Controls"
dat_skeleton[grep("Schizophrenics", dat_skeleton$Condition), "Group"]="Schizophrenics"
dat_skeleton[grep("Alzheimer", dat_skeleton$Condition), "Group"]="Alzheimer"

dat_skeleton$StimType=NA
dat_skeleton[grep("Explicature", dat_skeleton$Condition), "StimType"]="Explicature"
dat_skeleton[grep("Implicature", dat_skeleton$Condition), "StimType"]="Implicature"


table(dat_skeleton$StimType, dat_skeleton$Condition)
table(dat_skeleton$Group, dat_skeleton$Condition)

dat = dat_skeleton[, c("Subj_ID", "Item_ID", "TOM", "Interpretability", "Age", "Group", "StimType", "Ntrial", "RT")]
dat$Subj_ID=factor(dat$Subj_ID)
dat$Item_ID=factor(dat$Item_ID)
dat$StimType = factor(dat$StimType)
dat$Group = factor(dat$Group)

save(dat, file="data/example3_dat.RData")



mod2 = lmer(RT~StimType*Group+ StimType*Interpretability+TOM+Age+(1+StimType*Group|Subj_ID) + (1|Item_ID), data=dat)
summary(mod2)

plot(allEffects(mod2))
plot(effect(mod2, term="StimType*Interpretability"))


### NOT GOOD !!! Use better way to generate a binomial model 
dat_bin = dat
dat_bin$ACC = NA
#for (iS in Subjects){
#  perc_low = quantile(dat_bin[dat_bin$Subj_ID==iS, "RT"], prob=0.40)
#  perc_up = quantile(dat_bin[dat_bin$Subj_ID==iS, "RT"], prob=0.60)
#  dat_bin[dat_bin$Subj_ID==iS, "ACC"][dat_bin[dat_bin$Subj_ID==iS, "RT"]<=perc_low] = 1
#  dat_bin[dat_bin$Subj_ID==iS, "ACC"][dat_bin[dat_bin$Subj_ID==iS, "RT"] > perc_up] = 0
#  dat_bin[dat_bin$Subj_ID==iS&is.na(dat_bin$ACC), "ACC"] = sample(c(0,1), length(dat_bin[dat_bin$Subj_ID==iS&is.na(dat_bin$ACC), "ACC"]), replace=T)
#}
### NOTE with this data simulation, random intercept for Subjects become 0 because for all subjects
# RT are split into high vs low, separately for each subject.
# 
# perc_low = quantile(dat_bin$RT, prob=0.40)
# perc_up = quantile(dat_bin$RT, prob=0.60)
# dat_bin$ACC[dat_bin$RT<=perc_low] = 1
# dat_bin$ACC[dat_bin$RT>=perc_up] = 0
# dat_bin[is.na(dat_bin$ACC), "ACC"] = sample(c(0,1), length(dat_bin[is.na(dat_bin$ACC), "ACC"]), replace=T)
z = scale(dat_bin$RT)
pr = 1/(1+exp(-z)) 
dat_bin$ACC = rbinom(length(z),1, pr)
#https://stats.stackexchange.com/questions/46523/how-to-simulate-artificial-data-for-logistic-regression


table(dat_bin$ACC, dat_bin$Subj_ID, useNA="ifany")


mod2.glmer = glmer(ACC~StimType*Group+TOM+Interpretability+Ntrial+(1|Subj_ID)+(1|Item_ID), data=dat_bin, family="binomial")
summary(mod2.glmer)

mod2.glmer_af = allFit(mod2.glmer)
summary(mod2.glmer_af)

check_model(mod2.glmer)

save(dat_bin, file="data/example3bin_dat.RData")


