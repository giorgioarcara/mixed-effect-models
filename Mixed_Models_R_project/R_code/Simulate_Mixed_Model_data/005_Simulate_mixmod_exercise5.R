rm(list=ls())
library(lmerTest)
library(car)
library(faux)
library(effects)
set.seed(150)

##########################
# SIMULATE MIXED MODEL DATA
#######################################
# Author: Giorgio Arcara
# date: 05/2024
# This script contain some code to simulate data for mixed model
# I strongly based the code by the simulation by Lisa de Bruine (https://debruine.github.io/tutorials/sim-lmer.html),
# and by some lessons by Harald Baayen. Blame on my for all the mistakes.

# NOTE: this is similar to exercise 2, but with a non-linear effect for interpretability


#########################
# FIXED EFFECTS #########
#########################
# define population level fixed effect, for factor (use just one factor for all)
# and for covariates

# here for example I have two factors F1 and F2 with two levels each 
# so I put all the combinations in just one factor.

Cond_labels = c("Idiom_Easy", "Metaphor_Easy", "Proverb_Easy", "Idiom_Hard", "Metaphor_Hard", "Proverb_Hard")

# here the effect at population levels
Cond_effs = c(0, +30, +70, +30, +70, +120)

Cond_adj = data.frame(Cond_labels, Cond_effs)

# other relevant value to define final formula
Grand_Intercept = 10000

# sigma
mod_sigma = 20

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
hist(Subj_adj$Metaphor_Easy)


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
Item_n = 60 # number of Total Items  (20 stim for condition)
Item_sd = 80
Item_ID = 1:Item_n
Item_adj =  data.frame(Item_ID , 
                            Item_rInt = rnorm(Item_n, mean=0, sd=Item_sd)
)

## add some variables to items (not interacting with anything)
Item_adj$NumberOfLetters = rnorm(Item_n, mean = 10, sd = 1)
Item_adj$NumberOfLetters_adj = Item_adj$NumberOfLetters * 0.5

Item_adj$Concreteness = rnorm(Item_n, mean = 3, sd = 2) + 5 # (+5 is to avoid values below 0)
Item_adj$Concreteness_adj = -Item_adj$Concreteness * 3
## add interaction with one variable

## simulate  non-linear term
Item_adj$Interpretability = rnorm(Item_n, mean = 0, sd = 3) + 5
Item_adj$Interpretability_adj =  +Item_adj$Interpretability * 10 + -Item_adj$Interpretability ^2* 50


####################################
# PART 3 CREATE DATA.FRAME COMBINATION
####################################
# here you create a combination of Subject, Items, and Condition, to create the "Experiment"
dat_skeleton = expand.grid(Subj_ID = Subj_ID, Item_ID = Item_ID, Condition = Cond_labels)

# here I created all the possible combinations. However, depending from the experiment, this could make not sense.
# for example, I want Item from 1:100 are associated to F11, while Item from 101:200 to F12
# all Items are associated to F2.
# to do this, I remove the non relevant combination here.
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(1:20) & dat_skeleton$Condition%in%c("Metaphor_Easy", "Metaphor_Hard", "Proverb_Easy", "Proverb_Hard")), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(21:40) & dat_skeleton$Condition%in%c("Idiom_Easy", "Idiom_Hard", "Proverb_Easy", "Proverb_Hard")), ]
dat_skeleton  = dat_skeleton[!(dat_skeleton$Item_ID%in%c(41:60) & dat_skeleton$Condition%in%c("Idiom_Easy", "Idiom_Hard", "Metaphor_Easy", "Metaphor_Hard")), ]


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
                                                  "Concreteness_adj", "NumberOfLetters_adj", "Interpretability_adj",
                                                  "Error")], 1, sum)
head(dat_skeleton)

hist(dat_skeleton$RT)

mod1 = lmer(RT~Condition+Interpretability+Concreteness+NumberOfLetters+(1|Subj_ID) + (1|Item_ID), data=dat_skeleton)
vif(mod1)
plot(allEffects(mod1))
summary(mod1)

plot(effect(mod1, term="Interpretability", partial.residuals=TRUE))

mod2 = lmer(RT~Condition*Interpretability+Concreteness+NumberOfLetters+(1|Subj_ID) + (1|Item_ID), data=dat_skeleton)

summary(mod2)



# check correspondence of estimates with model 2
#plot(Subj_Cond_adj$SubjCond_adj[1:20], ranef(mod2)$Subj_ID$ConditionF11_F22)
#plot(Subj_rInt_adj$Subj_rInt, ranef(mod2)$Subj_ID[["(Intercept)"]])

# createa dataset 
dat_skeleton$StimType=NA
dat_skeleton[grep("Idiom", dat_skeleton$Condition), "StimType"]="Idiom"
dat_skeleton[grep("Metaphor", dat_skeleton$Condition), "StimType"]="Metaphor"
dat_skeleton[grep("Proverb", dat_skeleton$Condition), "StimType"]="Proverb"

dat_skeleton$Difficulty=NA
dat_skeleton[grep("Easy", dat_skeleton$Condition), "Difficulty"]="Easy"
dat_skeleton[grep("Hard", dat_skeleton$Condition), "Difficulty"]="Hard"


table(dat_skeleton$StimType, dat_skeleton$Condition)
table(dat_skeleton$Difficulty, dat_skeleton$Condition)

dat = dat_skeleton[, c("Subj_ID", "Item_ID", "Concreteness", "Interpretability", "NumberOfLetters", "Difficulty", "StimType", "Ntrial", "RT")]

save(dat, file="data/example5_dat.RData")


mod2 = lmer(RT~StimType*Difficulty+ StimType*Interpretability+Concreteness+NumberOfLetters+(1+StimType|Subj_ID) + (1|Item_ID), data=dat_skeleton)
plot(allEffects(mod2))
plot(effect(mod2, term="StimType*Interpretability"))

library(DHARMa)
simOut = simulateResiduals(fittedModel=mod2)
plot(simOut)

plotResiduals(simOut, dat$Subj_ID)
plotResiduals(simOut, dat$Interpretability)
plotResiduals(simOut, dat$StimType)


