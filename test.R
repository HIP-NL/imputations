library("ineq")     # inequality measures and example data
library("EnvStats") # for estimation of censored distributions

source("impute.R") 

# to test we manually truncate the Ilocos income data from the ineq package
data(Ilocos, package = "ineq")

# truncate incomes below 40000, but first get the number of missing
# observations this implies
n_missing = sum(Ilocos$income < 40e3)

# truncate distribution
y_trunc = Ilocos$income[Ilocos$income >= 40e3]

# a look at the two distributions
par(mfrow = c(1, 2))
xr = log(range(Ilocos$income))
hist(log(Ilocos$income), xlim = xr, breaks = 20)
hist(log(y_trunc), xlim = xr, breaks = 20)

# the unbiased estimates
ineq::Gini(Ilocos$income)
mean(log(Ilocos$income))
sd(log(Ilocos$income))

# the biased estimates due to truncation
ineq::Gini(y_trunc)
mean(log(y_trunc))
sd(log(y_trunc))

# estimate_lnorm() uses EnvStats::enormCensored() to get unbiased mean and sd
# for the trunc. lognormal
estimate_lnorm(y_trunc, n_missing)

# impute_from_sd() uses the sd from the lognormal to estimate the Gini
impute_from_sd(y_trunc, n_missing)

# sample_from_lognormal() uses the EnvStats::rnormTrunc() for random
# generation for the truncated normal distribution, which can then be used to
# calculate the Gini on
resampled = sample_from_lognormal(y_trunc, n_missing)
ineq::Gini(resampled)
mean(log(resampled))
sd(log(resampled))

# impute_from_subsistence fills in the missing observations with an estimate
# of subsistence income to calculate the Gini. Not implemented yet