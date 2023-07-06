import numpy as np
from scipy.stats import norm, truncnorm

from rpy2.robjects import r, globalenv
from rpy2.robjects.packages import importr
from rpy2.robjects.vectors import FloatVector, BoolVector

# Importing required R package
env_stats = importr('EnvStats')
# Importing required R packages

# def estimate_lnorm(y, n_missing):
#     '''   
#     take a truncated empirical distribution and estimate mean and sd as if it were lognormal
#     expects:
#       y: the untransformed data
#       n_missing: the expected number of missing observations
#     '''
#     censored = np.concatenate((np.repeat(False, len(y)), np.repeat(True, n_missing)))
#     y = np.concatenate((y, np.repeat(np.nanmin(y), n_missing)))
#     y = np.log(y)
    
#     uncensored_data = y[~censored]  # Values not censored
#     censored_data = y[censored]  # Values censored             
    
#     # Replace censored data with constant (-np.inf). Left from the below threshold
#     left_censored_data = np.repeat(-np.inf, len(censored_data))    
    
#     # Combine censored and uncensored data
#     full_data = np.concatenate((uncensored_data, left_censored_data))

#     # Use scipy's norm.fit function to estimate the mean and standard deviation
#     mean, std_dev = norm.fit(full_data)

#     return {'log_mean': mean, 'log_sd': std_dev}


def estimate_lnorm(y, n_missing):
    # take a truncated empirical distribution and estimate mean and sd as if it were lognormal
    # expects:
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations
    censored = np.concatenate([np.repeat(False, len(y)), np.repeat(True, n_missing)])

    y = np.concatenate([y, np.repeat(np.nanmin(y), n_missing)])
    y = y.astype(np.float64)

    y = np.log(y)

    # Converting numpy arrays to R vectors
    y_r = FloatVector(y)
    censored_r = BoolVector(censored)

    estimates = env_stats.enormCensored(y_r, censored = censored_r, censoring_side = "left")
    
    output_dict = dict(
        log_mean = estimates.rx2('parameters').rx2('mean')[0],
        log_sd = estimates.rx2('parameters').rx2('sd')[0]
    )

    return output_dict


def impute_from_sd(y, n_missing):
    # Aitchison and Brown gini from lnorm
    # Parameters:
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations
    
    parameters = estimate_lnorm(y, n_missing)
    sd_y = parameters['log_sd']
    gini = 2 * norm.cdf(sd_y / np.sqrt(2)) - 1
    
    return gini

def sample_from_lognormal(y, n_missing):
    # Fill NAs with resample from truncated lognormal distribution
    # Parameters:
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations
    
    parameters = estimate_lnorm(y, n_missing)
    
    imputations = truncnorm.rvs(
        a=float('-inf'),
        b=np.log(np.min(y)),
        loc=parameters['log_mean'],
        scale=parameters['log_sd'],
        size=n_missing
    )
    
    out = np.concatenate((y, np.exp(imputations)))
    
    return out


# def sample_from_lognormal(y, n_missing):
#     # fill NAs with resample from trunc lnorm
#     # expects
#     #   y: the untransformed data
#     #   n_missing: the expected number of missing observations

#     parameters = estimate_lnorm(y, n_missing)
    
#     imputations = env_stats.rnormTrunc(
#         n = n_missing, 
#         mean = parameters['log_mean'], 
#         sd = parameters['log_sd'], 
#         a = float('-inf'),
#         b = np.min(np.log(y)))

#     out = np.concatenate([y, np.exp(imputations)])

#     return out


def impute_from_subsistence(y, n_missing, y_subsistence):
    # Fill NAs with subsistence incomes from X
    # Parameters:
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations
    #   y_subsistence: subsistence income value
    
    out = np.concatenate((y, np.full(n_missing, y_subsistence)))
    
    return out
