estimate_lnorm = function(y, n_missing){
    # take a truncated empirical distribution and estimate mean and sd as if it were lognormal
    # expects:
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations
    censored = c(
        rep(FALSE, length(y)),
        rep(TRUE, n_missing)
    )

    # construct full vector with censored values. Censored values need to be the number to the left of which the true value is
    y_with_censored = c(
        y,
        rep(min(y, na.rm = TRUE), n_missing)
    )

    estimates = EnvStats::elnormCensored(
        y_with_censored,
        censored = censored,
        censoring.side = "left")
    return(list(
        log_mean = estimates$parameters["meanlog"],
        log_sd = estimates$parameters["sdlog"]))
}

impute_from_sd = function(y, n_missing){
    # aitchison and brown gini from lnorm
    # expects
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations

    parameters = estimate_lnorm(y, n_missing)
    sd_y = parameters$log_sd
    gini = 2*pnorm(sd_y / sqrt(2)) - 1

    return(
        setNames(gini, "gini")
    )
}

sample_from_lognormal = function(y, n_missing, min_val = 0){
    # fill NAs with resample from trunc lnorm
    # expects
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations


    parameters = estimate_lnorm(y, n_missing)

    imputations = rlnormTrunc(
        n = n_missing,
        mean = parameters$log_mean,
        sd = parameters$log_sd,
        min = log(min_val),
        max = min(log(y)))

    out = c(y, exp(imputations))

    return(out)
}

impute_from_subsistence = function(y, n_missing, y_subsistence){
    # fill NAs with some fixed (subsistence) incomes
    # tbd
     out = c(y, rep(y_subsistence, times = n_missing))

    return(out)
}
