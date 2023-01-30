library("EnvStats")
library("ineq")
library("data.table")

y = Ilocos$income

hist(y)
hist(log(y))
censored = y < 40e3
y_cens = ifelse(censored, 40e3, y)
hist(log(y_cens))


y_cens = log(y_cens)

mean(log(y))
sd(log(y))

e2 = EnvStats::enormCensored(y_cens, censored = censored, censoring.side = "left")
e2$parameters
mean(log(y_cens))
sd(log(y_cens))
mean(log(y))
sd(log(y))
