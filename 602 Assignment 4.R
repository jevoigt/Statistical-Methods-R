require(Sleuth3)
require(mosaic)
Faithful = read.csv("Faithful.csv")
summary(Faithful)
head(Faithful)

lm1 = lm(waiting~eruptions, data = Faithful)
summary(lm1)

plot(waiting~eruptions, data = Faithful)
abline(lm(waiting~eruptions, data = Faithful))
plot(lm1, which = 2)
plot(lm1, which = 1)

fitted(lm1)
resid(lm1)^2
SSR = sum(resid(lm1)^2)
SSR
n = count(Faithful)
n
sqrt((SSR)/(270))
confint(lm1)

beta0 = coef(lm1)["(Intercept)"]
beta0
beta1 = coef(lm1)["eruptions"]
beta1
sigma = summary(lm1)$sigma
sigma
mu = beta0 + beta1 * 5
mu

n = nrow(Faithful)
mean = mean(~eruptions, data = Faithful)
sd = sd(~eruptions, data = Faithful)
se = sigma * sqrt(1/n + (5-mean)^2/((n-1) * sd))
se
upper = mu + qt(0.975, df = 270) * se
upper
lower = mu - qt(0.975, df = 270) * se
lower
pred = beta0 + beta1 * 3
pred
predse = sigma * sqrt(1 + (1/n) + (3-mean)^2/(n-1)*sd)
predse
predupper = pred + qt(0.975, df = 270) * predse
predupper
predlower = pred - qt(0.975, df = 270) * predse
predlower

eruption.lm = lm(waiting~eruptions, data=Faithful) 
eruption = resid(eruption.lm)

eruption.lm = lm(eruptions ~ waiting, data=faithful) 
eruption.res = resid(eruption.lm)
plot(Faithful$waiting, eruption.res)
abline(0, 0)

ex0817
lm2 = lm(Mass~Load, data = ex0817)
summary(lm2)
plot(Mass~Load, data = ex0817)
abline(lm(Mass~Load, data = ex0817))
plot(lm2, which = 1)

logmass = log(ex0817$Mass)
plot(logmass~Load, data = ex0817)
abline(lmlogY)
lmlogY = lm(logmass~Load, data = ex0817)
summary(lmlogY)
plot(lmlogY, which = 1)


logload = log(ex0817$Load)
plot(Mass~logload, data = ex0817)
abline(lmlogX)
lmlogX = lm(Mass~logload, data = ex0817)
summary(lmlogX)
plot(lmlogX, which = 1)

lmlogYlogX = lm(logmass~logload, data = ex0817)
summary(lmlogYlogX)
plot(logmass~logload, data = ex0817)
abline(lm(logmass~logload, data = ex0817))
plot(lmlogYlogX, which = 1)


bees = ex0327
summary(bees)
bees = bees[-c(36:47),]
bees
lm3 = lm(PollenRemoved~DurationOfVisit, data = bees)
plot(PollenRemoved~DurationOfVisit, data = bees)
abline(lm(PollenRemoved~DurationOfVisit, data = bees))
plot(lm3, which = 1)
bees = bees[-c(14,34),]
bees
lm4 = lm(PollenRemoved~DurationOfVisit, data = bees)
plot(PollenRemoved~DurationOfVisit, data = bees)
abline(lm(PollenRemoved~DurationOfVisit, data = bees))
plot(lm4, which = 1)
