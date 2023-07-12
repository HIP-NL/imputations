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

def sample_from_lognormal_py(y, n_missing):
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


def sample_from_lognormal(y, n_missing):
    # fill NAs with resample from trunc lnorm
    # expects
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations

    parameters = estimate_lnorm(y, n_missing)
    
    imputations = env_stats.rnormTrunc(
        n = n_missing, 
        mean = parameters['log_mean'], 
        sd = parameters['log_sd'], 
        min = float('-inf'),
        max = float(np.min(np.log(y))))

    out = np.concatenate([y, np.exp(imputations)])

    return out


def impute_from_subsistence(y, n_missing, y_subsistence):
    # Fill NAs with subsistence incomes from X
    # Parameters:
    #   y: the untransformed data
    #   n_missing: the expected number of missing observations
    #   y_subsistence: subsistence income value
    
    out = np.concatenate((y, np.full(n_missing, y_subsistence)))
    
    return out



import numba

@numba.njit()
def random_class_incomes(min_values, max_values, counts):
    """Generate random income values for each class, based on the min and max values and the number of values to generate for each class"""
    
    # calculate total number of values to generate
    size = np.sum(counts)
    # create empty array
    all_class_incomes = np.empty(size)
    
    # generate random income values for each class, and add to all_class_incomes
    start = 0
    for i in range(len(min_values)):
        end = start + counts[i]
        all_class_incomes[start:end] = np.random.uniform(min_values[i], max_values[i], counts[i])
        start = end

    assert len(all_class_incomes) == counts.sum(), "Length of class_incomes should be equal to sum of income_class in class_table"

    return all_class_incomes


def random_class_incomes_wrapper(class_table):
    """Wrapper around random_class_incomes, to be able to input a pandas dataframe directly"""
    # convert class_table to numpy arrays
    min_values = np.array(class_table['min'].to_numpy(), dtype=np.float64)
    max_values = np.array(class_table['max'].to_numpy(), dtype=np.float64)
    counts = np.array(class_table['count'].to_numpy(), dtype=np.int64)
    # generate random incomes
    all_class_incomes = random_class_incomes(min_values, max_values, counts)
    return all_class_incomes


# @numba.jit(nopython=True)
def gini_coefficient(array):
    # array = np.array(array)
    n = array.shape[0]
    array_sorted = np.sort(array)
    index = np.arange(1, n+1)
    weighted_sum = (array_sorted * index).sum()
    
    gini = (2 * weighted_sum) / (n * array.sum()) - (n + 1) / n
    return gini

test_array = np.array((5 * [50]) + (10 * [100]) + (5 * [300]))
gini = gini_coefficient(test_array)



def impute_and_calc_gini(i, n_missing, class_table):
    simulated_incomes = random_class_incomes_wrapper(class_table)
    imputed = sample_from_lognormal(simulated_incomes, n_missing)
    gini = gini_coefficient(imputed)
    return gini, simulated_incomes, imputed
