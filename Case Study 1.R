install.packages("mosaic") # note the quotation marks
require(mosaic)
install.packages("Sleth3")
require("Sleuth3")
trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 3)

summary(case0101)
favstats(Score~Treatment, data = case0101)
histogram(~Score | Treatment, data = case0101)

with(subset(case0101, Treatment == "Extrinsic"), stem(Score, scale = 5))
with(subset(case0101, Treatment == "Intrinsic"), stem(Score, scale = 5))

aggregate(Score ~ Treatment, data = case0101, FUN = stem, scale = 5)

t.test(Score~Treatment, alternative = "two.sided",conf = 0.95, data = case0101)
summary(lm(Score~Treatment, data = case0101))
diffmeans = diff(mean(Score~Treatment, data = case0101))
diffmeans
numsim = 1000
nulldist = do(numsim) * diff(mean(Score~shuffle(Treatment), data = case0101))
confint(nulldist)

head(case0101)
boxplot(Score~Treatment, data = case0101)
t.test(Score~Treatment, var.equal=T, data = case0101)


#Sex Descrimination 
library(Sleuth3)
head(case0102, 100)
boxplot(Salary~Sex, data = case0102)
par(mfrow=c(2,1))
hist(case0102$Salary[case0102$Sex == "Male"], xlim = c(3500,8500), main = "YUP", xlab = "Male Salary")
hist(case0102$Salary[case0102$Sex == "Female"], xlim = c(3500,8500), main = "YEAH", xlab = "Female Salary")     
par(mfrow = c(1,1))
t.test(Salary~Sex, var.equal=T, data = case0102)
