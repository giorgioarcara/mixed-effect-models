rm(list=ls())
library(lmerTest)
library(faux)
library(effects)
set.seed(102)

##########################
# SIMULATE MIXED MODEL DATA
#######################################
# Author: Giorgio Arcara
# date: 07/2021
# This script contain some code to simulate data for mixed model
# I strongly based the code by the simulation by Lisa de Bruine (https://debruine.github.io/tutorials/sim-lmer.html),
# and by some lessons by Harald Baayen. Blame on me for all the mistakes.


#########################
# FIXED EFFECTS #########
#########################
# define population level fixed effect, for factor (use just one factor for all)
# and for covariates

# here for example I have two factors F1 and F2 with two levels each 
# so I put all the combinations in just one factor.

Cond_labels = c("F11_F21", "F12_F21", "F11_F22", "F12_F22")

# here the effect at population levels
Cond_effs = c(0, -50, 0, -20)

Cond_adj = data.frame(Cond_labels, Cond_effs)

# other relevant value to define final formula
Grand_Intercept = 600

# sigma
mod_sigma = 20

Dep_name = "RT"



#########################
# PART1 - SUBJECTS ######
#########################
# Define parameters of subjects, and subject-specific slopes
Subj_n = 20 # number of subjects

Subj_ID = 1:Subj_n

Subj_sd = 100 # the Subject random intercept (NOTE: it is an sd)

## add random slope here with no correlation with intercept
Subj_effsd = c(100, 100, 100) # note it is an sd
vars_cor = c(0.3) # correlations for RIntercept and Rslope
vars_n = 1+length(Subj_effsd) # Rand Intercept + Rand Slopes
Subj_adj = rnorm_multi(n=Subj_n, mu=0, r =vars_cor, 
                        sd = c(Subj_sd, Subj_effsd), varnames = c("Subj_rInt", Cond_labels[-1]))

# check that it worked
cor(Subj_adj)
hist(Subj_adj$F12_F21)


Subj_adj = cbind(Subj_ID, Subj_adj)
# note that I start from 3, which is the 
Subj_Cond_adj = reshape(Subj_adj, direction="long", idvar="Subj_ID", varying=list(3:(length(Subj_adj))), times = names(Subj_adj)[3:length(Subj_adj)])
names(Subj_Cond_adj) = c("Subj_ID", "Subj_rInt_adj", "Condition", "SubjCond_adj")
row.names(Subj_Cond_adj)=1:dim(Subj_Cond_adj)[1]



# now divide Subject Intercept from Subj random slope
#xyplot(SubjCond_adj~Subj_rInt_adj|Condition, Subj_Cond_adj)


Subj_Cond_adj$Subj_rInt_adj = NULL # delete the column with Random Intercept, to avoid confusion


# get random intercept here
Subj_rInt_adj = Subj_adj[, c("Subj_ID", "Subj_rInt")]  


##########################
# PART2 - ITEMS  ########
#########################
# Define parameters of Items, currently without Item-specific slopes
Item_n = 200 # number of Total Items 
Item_sd = 100
Item_ID = 1:Item_n
Item_adj =  data.frame(Item_ID , 
                            Item_rInt = rnorm(Item_n, mean=0, sd=Item_sd)
)

## add some variables to items (not interacting with anything)
Item_adj$Complexity = rnorm(Item_n, mean = 80, sd = 10)
Item_adj$Complexity_adj = Item_adj$Complexity * 0

Item_adj$Imageability = rnorm(Item_n, mean = 3, sd = 1)
Item_adj$Imageability_adj = -Item_adj$Imageability * 80

## simulate two highly correlated variabls 
Item_adj$Concreteness = Item_adj$Imageability + rnorm(length( Item_adj$Imageability), mean=0, sd = 0.1)
cor(Item_adj$Concreteness, Item_adj$Imageability)
Item_adj$Concreteness_adj =  -Item_adj$Concreteness * 60


####################################
# PART 3 CREATE DATA.FRAME COMBINATION
####################################
# here you create a combination of Subject, Items, and Condition, to create the "Experiment"
dat_skeleton = expand.grid(Subj_ID = Subj_ID, Item_ID = Item_ID, Condition = Cond_labels)

# here I created all the possible combinations. However, depending from the experiment, this could make not sense.
# for example, I want Item from 1:100 are associated to F11, while Item from 101:200 to F12
# all Items are associated to F2.
# to do this, I remove the non relevant combination here.
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(1:100)&dat_skeleton$Condition%in%c("F12_F21", "F12_F22")), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(101:200)&dat_skeleton$Condition%in%c("F11_F21", "F11_F22")), ]

table(dat_skeleton$Item_ID, dat_skeleton$Subj_ID)


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



# ADD DEPENDENT VARIABLE (combining everything)
dat_skeleton[, Dep_name] = apply(dat_skeleton[, c("Grand_Intercept", "Cond_effs", "Subj_rInt",
                                                  "SubjCond_adj", "Item_rInt", 
                                                  "Complexity_adj", "Imageability_adj", "Concreteness_adj",
                                                  "Error")], 1, sum)
head(dat_skeleton)

hist(dat_skeleton$RT)

mod1 = lmer(RT~Condition+Imageability+Complexity+(1|Subj_ID) + (1|Item_ID), data=dat_skeleton)
plot(allEffects(mod1))
summary(mod1)


#mod2 = lmer(RT~Condition+Imageability+Complexity+(1+Condition|Subj_ID) + (1|Item_ID), data=dat_skeleton)
#summary(mod2)

# check correspondence of estimates with model 2
#plot(Subj_Cond_adj$SubjCond_adj[1:20], ranef(mod2)$Subj_ID$ConditionF11_F22)
#plot(Subj_rInt_adj$Subj_rInt, ranef(mod2)$Subj_ID[["(Intercept)"]])

# createa dataset 
dat_skeleton$StimType=NA
dat_skeleton[grep("F11", dat_skeleton$Condition), "StimType"]="Metaphor"
dat_skeleton[grep("F12", dat_skeleton$Condition), "StimType"]="Literal"
dat_skeleton$PrimeType=NA
dat_skeleton[grep("F21", dat_skeleton$Condition), "PrimeType"]="Positive"
dat_skeleton[grep("F22", dat_skeleton$Condition), "PrimeType"]="Negative"


table(dat_skeleton$StimType, dat_skeleton$Condition)
table(dat_skeleton$PrimeType, dat_skeleton$Condition)

dat = dat_skeleton[, c("Subj_ID", "Item_ID", "Complexity", "Imageability", "Concreteness", "PrimeType", "StimType", "Ntrial", "RT")]
# reorder data
dat=dat[order(dat$Subj_ID, dat$Ntrial),]

### to simulate a simple binomial distribution I create (by subjects) ##
## that data above the 60 percent are = 0, and data below 40 percet are = 1
# all the others are randomly assigned to either 1 or 0.

dat_bin = dat
dat_bin$ACC = NA
for (iS in Subjects){
  perc_low = quantile(dat_bin[dat_bin$Subj_ID==iS, "RT"], prob=0.35)
  perc_up = quantile(dat_bin[dat_bin$Subj_ID==iS, "RT"], prob=0.75)
  dat_bin[dat_bin$Subj_ID==iS, "ACC"][dat_bin[dat_bin$Subj_ID==iS, "RT"]<=perc_low] = 1
  dat_bin[dat_bin$Subj_ID==iS, "ACC"][dat_bin[dat_bin$Subj_ID==iS, "RT"] > perc_up] = 0
  dat_bin[dat_bin$Subj_ID==iS&is.na(dat_bin$ACC), "ACC"] = sample(c(0,1), length(dat_bin[dat_bin$Subj_ID==iS&is.na(dat_bin$ACC), "ACC"]), replace=T)
}
table(dat_bin$ACC, dat_bin$Subj_ID, useNA="ifany")

save(dat, file="data/example1_dat.RData")
save(dat, file="data/example1bin_dat.RData")


