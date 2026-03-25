library("ineq")     # inequality measures and example data
library("EnvStats") # for estimation of censored distributions

source("impute.R")

calc_metrics = function(y) {
    list(
        gini = ineq::Gini(y),
        mean_log = mean(log(y)),
        sd_log = sd(log(y))
    )
}

# to test we manually truncate the Ilocos income data from the ineq package
data(Ilocos, package = "ineq")

# truncate incomes below 40000, but first get the number of missing
# observations this implies
n_missing = sum(Ilocos$income < 40e3)

# truncate distribution
y_trunc = Ilocos$income[Ilocos$income >= 40e3]

# impute distribution
imputed = sample_from_lognormal(y_trunc, n_missing)

# a look at the two distributions
par(mfrow = c(1, 3))
xr = log(range(Ilocos$income))
hist(log(Ilocos$income), xlim = xr, breaks = 20)
hist(log(y_trunc), xlim = xr, breaks = 20)
hist(log(imputed), xlim = xr, breaks = 20)
# procedure does not quite capture the skew of the original

# the unbiased estimates
calc_metrics(Ilocos$income)

# the biased estimates due to truncation
calc_metrics(y_trunc)

# the estimated parameters
estimate_lnorm(y_trunc, n_missing)

# impute by sampling lognormal
calc_metrics(imputed)

# impute gini directly
impute_from_sd(y_trunc, n_missing)
