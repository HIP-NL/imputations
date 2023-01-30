library("ineq")

source("impute.R")

n_missing = sum(Ilocos$income < 40e3)
y_trunc = Ilocos$income[Ilocos$income >= 40e3]
ineq::Gini(y_trunc)

ineq::Gini(Ilocos$income)
mean(log(Ilocos$income))
sd(log(Ilocos$income))

estimate_lnorm(y_trunc, n_missing)

impute_from_sd(y_trunc, n_missing)
resampled = sample_from_lognormal(y_trunc, n_missing)
ineq::Gini(resampled)
