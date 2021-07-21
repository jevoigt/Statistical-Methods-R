ex0126
mean(ex0126$Pro05, na.rm = T)
mean(ex0126$Anti05, na.rm = T)
mean(ex0126$Pro06, na.rm = T)
mean(ex0126$Anti06, na.rm = T)
mean(ex0126$Pro07, na.rm = T)
mean(ex0126$Anti07, na.rm = T)
mean(ex0126$PctPro, na.rm = T)
sd(ex0126$Pro05, na.rm = T)
sd(ex0126$Anti05, na.rm = T)
sd(ex0126$Pro06, na.rm = T)
sd(ex0126$Anti06, na.rm = T)
sd(ex0126$Pro07, na.rm = T)
sd(ex0126$Anti07, na.rm = T)
sd(ex0126$PctPro, na.rm = T)
n1 = length(ex0126$Pro05)
favstats(ex0126$Pro05)
favstats(ex0126$Anti05)
favstats(ex0126$Pro06)
favstats(ex0126$Anti06)
favstats(ex0126$Pro07)
favstats(ex0126$Anti07)
n1 = 437
n2 = 437
n3 = 434
n4 = 434
n5 = 435
n6 = 430
s1 = sd(ex0126$Pro05, na.rm = T)
s2 = sd(ex0126$Anti05, na.rm = T)
s3 = sd(ex0126$Pro06, na.rm = T)
s4 = sd(ex0126$Anti06, na.rm = T)
s5 = sd(ex0126$Pro07, na.rm = T)
s6 = sd(ex0126$Anti07, na.rm = T)
S = ((n1 - 1)*(s1)^2) + ((n2-1)*(s2)^2) + ((n3-1)*(s3)^2) + ((n4-1)*(s4)^2) + ((n5-1)*(s5)^2) + ((n6-1)*(s6)^2)
Sp = S/(n1+n2+n3+n4+n5+n6-6)
sqrt(Sp)
Sp05 = sqrt(((n1-1)*(s1)^2 + (n2-1)*(s2)^2)/(n1+n2-2))
Sp05         
Sp06 = sqrt(((n3-1)*(s3)^2 + (n4-1)*(s4)^2)/(n3+n4-2))       
Sp06
Sp07 = sqrt(((n5-1)*(s5)^2 + (n6-1)*(s6)^2)/(n5+n6-2))  
Sp07
Se05 = (Sp05)*(0.0676535291)
Se05           
Se06 = (Sp06)*(0.06782329983)              
Se06
Se07 = (Sp07)*(0.06800294111)
Se07
(mean(ex0126$Pro05, na.rm = T))-(mean(ex0126$Anti05, na.rm = T))
(1.96)*(Se05)
t.test.05 = t.test(ex0126$Pro05-ex0126$Anti05)
t.test.05
t.test(ex0126$Pro06-ex0126$Anti06)
t.test(ex0126$Pro07-ex0126$Anti07)


#Fish Oil
ex0112
sd(ex0112$BP[ex0112$Diet == "FishOil"])
SD.RO = sd(ex0112$BP[ex0112$Diet == "RegularOil"])
SE.RO = (SD.RO)/(sqrt(7))
(2.4469)*(SE.RO)

ex0126
-1.1429+2.9454
-1.1429-2.9454
(-1.1429)/(SE.RO)
t.test(ex0112$BP[ex0112$Diet == "RegularOil"])

#AFQT
ex0222
mean(ex0222$AFQT[ex0222$Gender == "male"])
mean(ex0222$AFQT[ex0222$Gender == "female"])
boxplot(ex0222$AFQT~ex0222$Gender, ylab = "AFQT Score")
t.test(ex0222$AFQT~ex0222$Gender)

R.mean = mean(ex0126$PctPro[ex0126$Party == "R"], na.rm = T)
D.mean = mean(ex0126$PctPro[ex0126$Party == "D"], na.rm = T)
R.sd = sd(ex0126$PctPro[ex0126$Party == "R"], na.rm = T)
D.sd = sd(ex0126$PctPro[ex0126$Party == "D"], na.rm = T)
R.mean
D.mean
R.sd
D.sd
length(ex0126$Party == "R")
length(ex0126$Party == "D")
summary(ex0126)
summary(R.mean)
Sp = sqrt(((246-1)*(R.sd)^2 + (243 - 1)*(D.sd)^2)/(246+243-2))
SE = Sp * sqrt(1/246 + 1/243)
SE
summary(ex0126$PctPro[ex0126$Party == "R"], na.rm = T)
summary(ex0126$PctPro[ex0126$Party == "D"], na.rm = T)
ex0126
(1.96)*(0.38497)
69.725+0.75454
69.725-0.75454
t.test(ex0126$PctPro[ex0126$Party == "R"]~ ex0126$PctPro[ex0126$Party == "D"])
t.test(ex0126$PctPro[ex0126$Party == "R"]~ ex0126$PctPro[ex0126$Party == "D"])
t.test(ex0126$PctPro~ex0126$PartyWOI)
str(ex0126)
Y1 = R.mean
Y2 = D.mean
Yd = Y2 - Y1
Yd
qt = qt(0.975, 487)
qt
hw = qt*SE
hw
lower = Yd - hw
lower
upper = Yd + hw
upper
