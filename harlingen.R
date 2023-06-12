rm(list = ls())

library("data.table")
library("arrow")
library("ineq")     # inequality measures and example data
library("EnvStats") # for estimation of censored distributions

source("impute.R") 

mypar = function(...){
    par(..., 
        bty = "l", 
        mar = c(4, 3, 2, 1), 
        mgp = c(1.7, .5, 0), 
        tck=-.01,
        font.main = 1)
}

tax = read_parquet("~/Downloads/tax_records_latest.parquet")
setDT(tax)

tax = tax[place == "Harlingen" & year == 1889]
y_trunc = na.omit(tax$income_taxable)

n_missing = c(m30 = 500, m40= 700, m50 = 1000, m60 = 1500, m70 = 2000, m80 = 3000)
1000 / (n_missing + 1000)

# naive estimates
ineq::Gini(y_trunc)
mean(log(y_trunc), na.rm = TRUE)
sd(log(y_trunc), na.rm = TRUE)

# each method, 6 n_missing, and for impute from subs. 3 subsistence estimate

# estimate_lnorm() uses EnvStats::enormCensored() to get unbiased mean and sd
# for the trunc. lognormal
sapply(n_missing, estimate_lnorm, y = y_trunc)

# impute_from_sd() uses the sd from the lognormal to estimate the Gini
impute_from_sd(y_trunc, n_missing[1])
toplot_from_sd = sapply(n_missing, impute_from_sd, y = y_trunc)

# sample_from_lognormal() uses the EnvStats::rnormTrunc() for random sample
# from truncated normal distribution, which is used to calculate the Gini

# list of samples
resampled = lapply(n_missing, sample_from_lognormal, y = y_trunc)
# gini on each sample in the list
toplot_resampled = sapply(resampled, ineq::Gini)

subsistence500 = lapply(n_missing, impute_from_subsistence, y = y_trunc, y_subsistence = min(y_trunc))
subsistence400 = lapply(n_missing, impute_from_subsistence, y = y_trunc, y_subsistence = 400)
subsistence300 = lapply(n_missing, impute_from_subsistence, y = y_trunc, y_subsistence = 300)

toplot500 = sapply(subsistence500, ineq::Gini)
toplot400 = sapply(subsistence400, ineq::Gini)
toplot300 = sapply(subsistence300, ineq::Gini)

xval = n_missing / (1000 + n_missing)
mypar(mfrow = c(1, 3))
yl = c(0.3, 0.8)
plot(xval, toplot_from_sd, type = "b", pch = 19, col = 2, 
    ylim = yl, 
    ylab = "Gini", xlab = "share missing", main = "σ-method")
abline(h = ineq::Gini(y_trunc), col = "gray")
plot(xval, toplot_resampled, type = "b", pch = 19, col = 2, 
    ylim = yl, 
    ylab = "Gini", xlab = "share missing", main = "Resampled lnorm")
abline(h = ineq::Gini(y_trunc), col = "gray")
plot(xval, toplot500, type = "b", pch = 19, col = 2, 
    ylim = yl, 
    ylab = "Gini", xlab = "share missing", main = "Hard-impute subsistence")
abline(h = ineq::Gini(y_trunc), col = "gray")
lines(xval, toplot400, type = "b", pch = 19, col = 3)
lines(xval, toplot300, type = "b", pch = 19, col = 4)
legend("topright", fill = 2:4, legend = c(500, 400, 300))