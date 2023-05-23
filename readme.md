## Imputations

This repository provides functions, data, and examples to impute missing data for the estimation of inequality statistics such as the Gini index.

It's not a proper package yet, so you just have to manually use the R files, and make sure the dependencies are installed (`EnvStats` and `Ineq`).

`impute.R` contains the functions needed to do the imputations.

`test.R` provides examles how to use the imputation functions, but also tests the procedures on a dataset from the `ineq` library.

`wages.R` playes around with a number of datasets (`dat/amsterdam.xls` and `dat/factor prices and wage income.csv`) to estimate subsistence income in the Netherlands in the second half of the nineteenth century.