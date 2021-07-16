rm(list=ls())
load("data/example1_dat.RData")
library(lmerTest)
library(effects)
library(car)
library(emmeans)

# the following code refers to an hypothetical experiment with 20 subjects and 200 stimuli 
# (100 Metaphors and 100 Literal sentences). Stimuli are presented in two Stimulus tyopes (StimeType=Metaphor, StimType=Literal), 
# and two Priming type (PrimeType=egative and PrimeType=Positive).
# due to a mistakes in the simulation, the number of combination of PrimeType and StimulusType is not 50 per combination, but slightly different.




mod1.lm = lm(RT ~ StimType*PrimeType+Imageability+Complexity,
                     data=dat)
summary(mod1.lm)

Anova(mod1.lm, type="III")

pairs(emmeans(mod1.lm, ~StimType*PrimeType))


 
#acf_resid(mod.lm, split_pred = "PrimeType")
png("Figures/ex1_mod_lm.png", height=1600, width=1800, res=200)
plot(allEffects(mod1.lm))
dev.off()


png("Figures/BySubj_Boxplot.png", height=1600, width=1800, res=200)
boxplot(RT~Subj_ID, dat)
dev.off()

png("Figures/ByItem_Boxplot.png", height=1600, width=3000, res=200)
boxplot(RT~Item_ID, dat[dat$Item_ID<50,])
dev.off()

library(lme4)
 
mod.lmer1 = lmer(RT ~ StimType*PrimeType + Imageability + Complexity +
                  (1 | Subj_ID) + (1 | Item_ID),
data=dat)


summary(mod.lmer1)
plot(allEffects(mod.lmer1))

library(lattice)
xyplot(RT~Imageability|Subj_ID, dat)


### visualize potential relevance of random slopes

# visualize random slopes
dat.subj_agg = aggregate(RT~Subj_ID+StimType, dat, mean)
StimType_mean = with(dat, tapply(RT, StimType, mean))

png("Figures/By_subj_slopes.png", height=800, width=800, res=200)
with(dat.subj_agg, interaction.plot(x.factor = StimType, trace.factor = Subj_ID, response = RT, legend=F))
lines(c(1,2), StimType_mean, col="red", lwd=5)
dev.off()


png("Figures/ex1_mod_lmer1.png", height=1600, width=1800, res=200)
plot(allEffects(mod.lmer1))
dev.off()

##### MODEL WITH RANDOM SLOPES
mod.lmer2 = lmer(RT ~ StimType*PrimeType + Imageability + Complexity +
                  (1 + StimType| Subj_ID) + (1 | Item_ID),
                data=dat)

summary(mod.lmer2)
plot(allEffects(mod.lmer2))


##### MODEL WITH RANDOM SLOPES
mod.lmer2a = lmer(RT ~ StimType*PrimeType + Imageability + Complexity +
                    (1 + Imageability |Subj_ID) + (1 | Item_ID),
                 data=dat)

summary(mod.lmer2a)
plot(allEffects(mod.lmer2a))


ranef(mod.lmer2)

png("Figures/ex1_mod_lmer2.png", height=1600, width=1800, res=200)
plot(allEffects(mod.lmer2))
dev.off()

## MODEL WITH MORE COMPLEX RANDOM SLOPES
mod.lmer3 = lmer(RT ~ StimType*PrimeType + Imageability + Complexity +
                   (1 + StimType*PrimeType| Subj_ID) + (1 | Item_ID),
                 data=dat)
summary(mod.lmer3)
plot(allEffects(mod.lmer3))

png("Figures/ex1_mod_lmer3.png", height=1600, width=1800, res=200)
plot(allEffects(mod.lmer3))
dev.off()

# MODEL WITH EVEN MORE COMPLEX RANDOM SLOPE
mod.lmer4 = lmer(RT ~ StimType*PrimeType + Imageability + Complexity +
                   (1 + StimType*PrimeType| Subj_ID) + (1 + Imageability | Subj_ID) + (1 | Item_ID),
                 data=dat)
summary(mod.lmer4)
plot(allEffects(mod.lmer4))

png("Figures/ex1_mod_lmer4.png", height=1600, width=1800, res=200)
plot(allEffects(mod.lmer4))
dev.off()


mod.lmer5 = lmer(RT ~ StimType*PrimeType*Imageability + Complexity +
                   (1 + StimType*PrimeType*Imageability | Subj_ID) + (1 | Item_ID),
                 data=dat)
summary(mod.lmer5)
plot(allEffects(mod.lmer5))

# example with uncorrelated random slopes and random intercept
mod.lmer6 = lmer(RT ~ StimType*PrimeType + Imageability +
                   (1 | Subj_ID) + (0 + StimType | Subj_ID) + (1 | Item_ID),
                 data=dat)
summary(mod.lmer6)
plot(allEffects(mod.lmer6))



mod.lmer7 = lmer(RT ~ StimType*PrimeType + Imageability + Complexity + Concreteness +
                   (1| Subj_ID) + (1 | Item_ID),
                 data=dat)


vif(mod.lmer7) # if VIF > 10, then harmful collinearity

## inspecting random effects
ranef(mod.lmer1)



##### COMPARE MODELS WITH LRT (fixed effects)
mod.lmer8a = lmer(RT ~ StimType*PrimeType +  
                  (1| Subj_ID) + (1 | Item_ID),
                  data=dat)


mod.lmer8b = lmer(RT ~ StimType*PrimeType + Imageability
                  (1| Subj_ID) + (1 | Item_ID),
                  data=dat)


anova(mod.lmer8a, mod.lmer8b)
# the models are nested, no significant effect, so the simpler model is selected.

# COMPARE MODELS WITH LRT (random effects)
mod.lmer8c = lmer(RT ~ StimType*PrimeType + Imageability +
                  (1 + StimType| Subj_ID) + (1 | Item_ID),
                  data=dat)


anova(mod.lmer8a, mod.lmer8c)
# there is a significant effect, the more complex random structure is better.


### COMPARE NON NESTED MODELS
mod.lmer8d = lmer(RT ~ StimType*PrimeType + Concreteness + Complexity +
                   (1| Subj_ID) + (1 | Item_ID),
                 data=dat)

AIC(mod.lmer8a)
AIC(mod.lmer8d)
# the lower AIC (Akaike Information Criterion), the better the model.

### STEPWISE SELECTION
mod.lmer.step4=step(mod.lmer4)
final.mod.lmer4 = get_model(mod.lmer.step4)


