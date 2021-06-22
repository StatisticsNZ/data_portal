# Number of cases (other countries)
source("scripts/updates/johnHopkins_globalCases.R")

# Electricity grid demand +++ Electricity grid demand by region
source("scripts/updates/EMA_grid.R")

# Broadband usage
source("scripts/updates/chorus_daily.R")

# Daily financial: Trade weighted index +++ Bank bill yields +++ Foreign exchange
source('scripts/updates/daily_financial.R')



# LOAD -----------------------------------------------------
source("scripts/run_load_process.R")
