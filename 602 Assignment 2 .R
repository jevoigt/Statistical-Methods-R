install.packages("mosaic")
require(mosaic)
install.packages("Sleuth3")
require(Sleuth3)
in_state = c(1000,4000,5000,8000,40000)
mean(in_state)
mean(log(in_state))
log(mean(in_state))
median(in_state)
median(log(in_state))
log(5000)
ratio = c(1,2,3,4,6)
median(ratio)
out_state = c(3000,8000,30000,32000,40000)
logs_both = log(out_state) - log(in_state)
logs_both
log(8.517/10.3090)
log(0.3333)

#Ex20 p 80
times_26kv = c(5.79, 1579.52, 2323.70)
times_28kv = c(68.8, 108.29, 110.29, 426.07, 1067.6)
Y1 = log(times_26kv)
Y2 = log(times_28kv)
Y1
Y2
sd(Y1)
sd(Y2)
mean(Y1)
mean(Y2)
mean(Y1) - mean(Y2)
Y1-Y2
exp(0.295)
T1 = mean(times_26kv)
T2 = mean(times_28kv)
t.test(Y1, Y2)
exp(-7.377556)
exp(7.966745)
exp(8.92)

install.packages("readxl")
library('readxl')
fusion = read_excel("fusion.xls")
View(fusion)
boxplot(Time~condition, data = fusion)
boxplot(log(Time)~condition, data = fusion)
sd(log(fusion$Time[fusion$condition == "1"]))
sd(log(fusion$Time[fusion$condition == "2"]))
mean(fusion$Time[fusion$condition == "1"])
mean(fusion$Time[fusion$condition == "2"])     
sd(fusion$Time[fusion$condition == "1"])
sd(fusion$Time[fusion$condition == "2"])   
hist(fusion$Time[fusion$condition == "1"])
hist(fusion$Time[fusion$condition == "2"])
t.test(Time~condition, var.equal = T, data = fusion)
fusion2 = fusion %>% slice(-c(1,44,45,46,47))
View(fusion2)
t.test(Time~condition, var.equal = T, data = fusion2)
t.test(log(Time)~condition, data = fusion)
exp(0.431)
exp(0.060)
exp(0.801)
#4
obsrank = rank(fusion$Time, ties.method = "average")    #Ask about this
obsrank
mt = sum(obsrank[1:43])
mt
average = mean(obsrank)
average
sd = sd(obsrank)
sd
n = nrow(subset(fusion, condition == "1"))
n
MEANT = n*average
MEANT
SDT = sd * sqrt((n^2)/2*n)
SDT
z = (mt-MEANT)/SDT 
z
p = pnorm(-abs(z))
p
wilcox.test(Time ~ condition, conf.int = TRUE, exact = TRUE, data = fusion)
stats = read.csv("statistics_opinions.csv")
t.test(stats$before, stats$after)
install.packages("coin")
require(coin)
as.numeric(stats$before)
as.numeric(stats$after)
wilcox.test(stats$before,stats$after, paired=T)    #Ask about this
wilcoxsign_test(after ~ before, distribution="exact", alternative = "greater", data=stats, 
                zero.method = "Wilcoxon")


