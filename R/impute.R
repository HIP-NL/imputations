#' Estimate Log-Normal Parameters from Censored Data
#'
#' Estimates the mean and standard deviation of the underlying log-normal distribution
#' from left-censored data.
#'
#' @param y Numeric vector. The observed (untransformed) data.
#' @param n_missing Integer. The number of missing (censored) observations.
#'
#' @return A list containing:
#' \describe{
#'   \item{log_mean}{The estimated mean of the log-transformed data.}
#'   \item{log_sd}{The estimated standard deviation of the log-transformed data.}
#' }
#' @importFrom EnvStats elnormCensored
#' @export
estimate_lnorm <- function(y, n_missing) {
    # take a truncated empirical distribution and estimate mean and sd as if it were lognormal
    censored <- c(
        rep(FALSE, length(y)),
        rep(TRUE, n_missing)
    )

    # construct full vector with censored values. Censored values need to be the number to the left of which the true value is
    y_with_censored <- c(
        y,
        rep(min(y, na.rm = TRUE), n_missing)
    )

    estimates <- EnvStats::elnormCensored(
        y_with_censored,
        censored = censored,
        censoring.side = "left"
    )
    return(list(
        log_mean = estimates$parameters["meanlog"],
        log_sd = estimates$parameters["sdlog"]
    ))
}

#' Estimate Gini Coefficient from Log-Normal SD
#'
#' Calculates the Gini coefficient based on the estimated standard deviation
#' of the log-normal distribution fitted to the censored data.
#'
#' @param y Numeric vector. The observed (untransformed) data.
#' @param n_missing Integer. The expected number of missing observations.
#'
#' @return A numeric vector of length 1 containing the estimated Gini coefficient.
#' @importFrom stats pnorm setNames
#' @export
impute_from_sd <- function(y, n_missing) {
    parameters <- estimate_lnorm(y, n_missing)
    sd_y <- parameters$log_sd
    gini <- 2 * pnorm(sd_y / sqrt(2)) - 1

    return(
        setNames(gini, "gini")
    )
}

#' Impute Missing Values by Sampling from Truncated Log-Normal
#'
#' Reconstructs the missing lower tail of the income distribution by resampling
#' from a truncated log-normal distribution fitted to the observed data.
#'
#' @param y Numeric vector. The observed (untransformed) data.
#' @param n_missing Integer. The expected number of missing observations.
#' @param min_val Numeric. The lower bound for the truncation (on the original scale). Defaults to 0.
#'
#' @return A numeric vector containing the original observed values `y` and the imputed values.
#' @importFrom EnvStats rnormTrunc
#' @export
sample_from_lognormal <- function(y, n_missing, min_val = 0) {
    parameters <- estimate_lnorm(y, n_missing)

    # We sample from the Normal distribution (log-scale)
    imputations <- EnvStats::rnormTrunc(
        n = n_missing,
        mean = parameters$log_mean,
        sd = parameters$log_sd,
        min = log(min_val), # lower bound on log scale
        max = min(log(y)) # upper bound (truncation point) on log scale
    )

    out <- c(y, exp(imputations))

    return(out)
}

#' Impute Missing Values with Subsistence Income
#'
#' Fills missing values with a fixed subsistence income value.
#'
#' @param y Numeric vector. The observed (untransformed) data.
#' @param n_missing Integer. The expected number of missing observations.
#' @param y_subsistence Numeric. The subsistence income value to use for imputation.
#'
#' @return A numeric vector containing the original observed values `y` and the imputed values.
#' @export
impute_from_subsistence <- function(y, n_missing, y_subsistence) {
    out <- c(y, rep(y_subsistence, times = n_missing))
    return(out)
}
