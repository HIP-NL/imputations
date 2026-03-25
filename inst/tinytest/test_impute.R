# Check if ineq is available for data
if (requireNamespace("ineq", quietly = TRUE)) {
    data(Ilocos, package = "ineq")
    y_true <- Ilocos$income
    truncation_point <- 40e3

    # Create truncated dataset
    n_missing <- sum(y_true < truncation_point)
    y_trunc <- y_true[y_true >= truncation_point]

    # --- Test: estimate_lnorm ---
    est <- estimate_lnorm(y_trunc, n_missing)

    expect_true(is.list(est))
    expect_true("log_mean" %in% names(est))
    expect_true("log_sd" %in% names(est))
    expect_true(is.numeric(est$log_mean))
    expect_true(is.numeric(est$log_sd))

    # --- Test: impute_from_sd ---
    gini_est <- impute_from_sd(y_trunc, n_missing)

    expect_true(is.numeric(gini_est))
    expect_true(length(gini_est) == 1)
    expect_true(gini_est >= 0 && gini_est <= 1)
    expect_equal(names(gini_est), "gini")

    # --- Test: sample_from_lognormal ---
    set.seed(42)
    y_imp_lnorm <- sample_from_lognormal(y_trunc, n_missing)

    # Check length
    expect_equal(length(y_imp_lnorm), length(y_true))

    # Check that observed values are preserved (first part of vector)
    expect_equal(y_imp_lnorm[1:length(y_trunc)], y_trunc)

    # Check that imputed values are below the minimum observed value
    # (Since we truncate at min(log(y)))
    imputed_part <- y_imp_lnorm[(length(y_trunc) + 1):length(y_imp_lnorm)]
    expect_true(all(imputed_part <= min(y_trunc)))

    # --- Test: impute_from_subsistence ---
    sub_val <- 30e3
    y_imp_sub <- impute_from_subsistence(y_trunc, n_missing, sub_val)

    expect_equal(length(y_imp_sub), length(y_true))
    # Check that the tail contains the subsistence value
    expect_true(all(tail(y_imp_sub, n_missing) == sub_val))
}
