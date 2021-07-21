library(Sleuth3)
library(ggplot2)
head(ex0327,50)

Subject = c(1:47)
ex0327$Subject = Subject


#Plots
ggplot(ex0327, aes(x = log(DurationOfVisit), y = PollenRemoved)) +
  geom_point(aes(color=as.factor(BeeType)))

lm1 = lm(PollenRemoved~log(DurationOfVisit) + BeeType +log(DurationOfVisit)*BeeType, data = ex0327)
summary(lm1)

plot(lm1, which = 1)

library(MASS)
ex0327$studres = rstudent(lm1)
ex0327$hat = hatvalues(lm1)
ex0327$cooks = cooks.distance(lm1)
plot(cooks~Subject, data = ex0327)
abline(h = 1)


plot(studres ~ Subject, data=ex0327)
abline(h=c(-2,2))

plot(hat ~ Subject, data=ex0327)
abline(h=2*2/47)


MV = c("None", "A", "B", "C", "AB", "AC", "BC", "ABC")
RSS = c(8100, 6240, 5980, 6760, 5500, 5250, 5750, 5160)
DOF = c(27,26,26,26,25,25,25,24)
df = data.frame(MV, RSS, DOF)
df

8100/27
6240/26
5980/26
6760/26
5500/25
5250/25
5750/25
5160/24








EV = c(300, 240, 230, 260, 220, 210, 230,215)
df$EV = EV
df

6240/8100
RSS/8100


R2 = c(0, 0.7704, 0.7383, 0.8346, 0.679, 0.6481, 0.7099, 0.637)
df$R2 = R2
df

EVtotal = 300

n = 28


Cp2 = 2 + (n-2)*((240/300)/(300))
Cp2
Cp3 = 2 + (n-2)*((230/300)/(300))
Cp3
Cp4 = 2 + (n-2)*((260/300)/(300))
Cp4
Cp5 = 3 + (n-3)*((220/300)/(300))
Cp5
Cp6 = 3 + (n-3)*((210/300)/(300))
Cp6
Cp7 = 3 + (n-3)*((230/300)/(300))
Cp7
Cp8 = 4 + (n-4)*((215/300)/(300))
Cp8

Cp = c(0, 2.0693, 2.0664, 2.0751, 3.0611, 3.0583, 3.0639, 4.0573)
df$Cp = Cp
df

BIC1 = n*(log(240)) + (2 + 1)*log(n)
BIC1
BIC2 = n*(log(230)) + (2 + 1)*log(n)
BIC2
BIC3 = n*(log(260)) + (2 + 1)*log(n)
BIC3
BIC4 = n*(log(220)) + (3 + 1)*log(n)
BIC4
BIC5 = n*(log(210)) + (3 + 1)*log(n)
BIC5
BIC6 = n*(log(230)) + (3 + 1)*log(n)
BIC6
BIC7 = n*(log(215)) + (4 + 1)*log(n)
BIC7

BIC = c(0, 163.4545, 162.2628, 165.6957, 164.3504, 163.0478, 165.595, 167.0389)

df$BIC = BIC
df

ex1123



