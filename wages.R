library("readxl")
library("data.table")

allen = readxl::read_xls("dat/amsterdam.xls",
    sheet = "Wages",
    skip = 7)

names(allen) = c("year", "drop", "meester", "knecht", "ongeschoold", "opperman", "houtzager")
setDT(allen)

riel = fread("dat/factor prices and wage income.csv",
    dec = ",",
    skip = 4)
riel = riel[, c(1, 7:9)]

names(riel) = c("year", "wage_agriculture", "wage_industry", "household_income")

# cf. the household expenditures from working-class budgets from van Riel
# 1853-63 1870-72 1880-82 1886-97 1910-11
# 442.23  358.84  587.36  584.30  887.91
riel[year %in% c(1853, 1871, 1881, 1910)]

# allen in stuivers per dag
# van riel in cents per day except household income which is fl.

# to get annual estimates van riel uses 300 days; allen 250, but typically we
# think that working days were higher in c19 so we go with van riel here
riel[, wage_industry := (wage_industry / 100) * 300]
riel[, wage_agriculture := (wage_agriculture / 100) * 300]
allen[, wage_urban_skilled := (meester / 20) * 300]
allen[, wage_urban_unskilled := (ongeschoold / 20) * 300]

wages = merge(riel, 
    allen[, list(year, wage_urban_skilled, wage_urban_unskilled)],
    all.x = TRUE)
wages = wages[year > 1850]

# in 1877 the minimum taxable income in A'dam was fl. 600 (de Meere)
wages[year == 1877]
# so here the household income would work
# overall something like 800 would not be out of the ordinary for the end of our period
fwrite(wages, "dat/wages.csv")
