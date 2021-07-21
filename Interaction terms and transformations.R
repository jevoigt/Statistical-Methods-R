require(Sleuth3)
require(mosaic)
ex0914


pairs(~Heart+Bank+Walk+Talk, upper.panel = panel.smooth, lower.panel = panel.smooth, data = ex0914)
Reg_heart = lm(Heart~Bank+Walk+Talk, data = ex0914)
summary(Reg_heart)
plot(Reg_heart, which = 1)

ex0327
BeeType = ex0327$BeeType
xyplot(PollenRemoved~DurationOfVisit, groups=BeeType, auto.key = T, data = ex0327)


logit(ex0327$PollenRemoved)
logit_pollen_removed = logit(ex0327$PollenRemoved)
log_duration = log(ex0327$DurationOfVisit)
summary(logit_duration)

xyplot(logit_pollen_removed~DurationOfVisit, groups = BeeType, auto.key = T, data = ex0327) 
xyplot(logit_pollen_removed~log_duration, groups = BeeType, auto.key = T, data = ex0327)

#Check email to see if you need to use tranformed variables for lm2 and lm3

lm1 = lm(logit_pollen_removed~log_duration + BeeType + log_duration*BeeType, data = ex0327)
lm2 = lm(logit_pollen_removed~log_duration + BeeType, data = ex0327)

summary(lm1)
summary(lm2)

ex0918

require(tidyverse)

ex0918 = ex0918 %>%
  gather("Males", "Females", key = "Sex", value = "Wingsize")
ex0918

xyplot(Wingsize~Latitude, groups = Continent,auto.key = T, data = ex0918)
lm3 = lm(Ratio~Latitude+Sex+Continent+Sex*Continent + Sex*Latitude, data = ex0918)
summary(lm3)




ex1014
lm4 = lm(Protein~Copper + Zinc, data = ex1014)
summary(lm4)
plot(lm4, which = 1)
lm5 = lm(log(Protein)~Copper + Zinc, data = ex1014)
summary(lm5)
plot(lm5, which = 1)
xyplot(Protein~Zinc, data = ex1014)

ex0722

ex0722$Species = relevel(ex0722$Species, ref = "Hemigrapsus nudus")
lm4 = lm(log(Force)~log(Height) + Species, data = ex0722)
lm5 = lm(Force~Height + Species, data = ex0722)
summary(lm5)
summary(lm4)

b2.hat <- lm4$coefficients[3]
b3.hat <- lm4$coefficients[4]
b2.hat - b3.hat
beta.cov <- vcov(lm4)
beta.cov

SE.b2.b3 <- sqrt(beta.cov[3,3]+beta.cov[4,4]-2*beta.cov[3,4])
SE.b2.b3

library(gmodels)
estimable(lm4, c(0,0,1,-1), conf.int=0.95)

log_height = log(ex0722$Height)

lm6 = lm(log(Force)~log_height + Species + log_height*Species, data = ex0722)
summary(lm6)
estimable(lm6, c(0,0,0,0,1,-1), conf.int=0.95)


lm.crab.rich = lm(log(Force)~log_height + Species + log_height*Species, data = ex0722)
summary(lm.crab.rich)
anova(lm.crab.rich)
lm.crab = lm(log(Force)~log_height + Species, data = ex0722)
summary(lm.crab)
anova(lm.crab)

# ESS test
anova(lm.crab,lm.crab.rich)


case0902
lm.brain.rich = lm(Brain~Body+Gestation+Litter+Gestation*Litter, data = case0902)
lm.brain = lm(Brain~Body+Gestation+Litter, data = case0902)
summary(lm.brain.rich)
summary(lm.brain)
anova(lm.brain.rich)
anova(lm.brain, lm.brain.rich)
(summary(lm.brain.rich)$sigma)**2


ex0915
xyplot(Yield~Rainfall+(Rainfall^2), data = ex0915)
corn.lm = lm(Yield~Rainfall+I(Rainfall^2)+Year, data=ex0915)
summary(corn.lm)
(-5.67038)/(2*-0.21550)
