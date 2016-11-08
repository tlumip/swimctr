# Code snippets that we can use to run a function in standalone mode

# Use for every module
library(tidyverse); library(doParallel)
DATA-RAW <- "../data-raw"
cluster_log = "./ct_cluster.log"
source("../R/ct_helpers.R")

# create_annual_faf_truckloads
this_year <- 2018
faf_data <- readr::read_csv(file.path(DATA-RAW, "faf42-oregon.csv.gz"))
truck_allocation_factors <- readr::read_csv(file.path(DATA-RAW, "faf35-truck-allocation-factors.csv"))
truck_equivalency_factors <- readr::read_csv(file.path(DATA-RAW, "faf35-truck-equivalency-factors.csv"))
empty_truck_factors <- readr::read_csv(file.path(DATA-RAW, "faf35-empty-truck-factors.csv"))
save_to <- "annual_faf_truckloads.csv"

# allocate_faf_to_zones
daily_faf_trips <- readr::read_csv("daily-faf-truckloads.csv")
makeuse <- readr::read_csv("makeuse-coefficients.csv")
synthetic_firms <- readr::read_csv("synthetic-firms.csv")
intermodal_connectors <- readr::read_csv(file.path(DATA-RAW, "ct-intermodal-connectors.csv"))
external_gateways <- readr::read_csv(file.path(DATA-RAW, "ct-external-station-equivalencies.csv"))
save_to <- "daily_faf_trips.csv"

# create_makeuse_coefficients
pecas_makeuse <- readr::read_csv("../tests/MakeUse.csv")
faf_data <- readr::read_csv(file.path(DATA-RAW, "faf42-oregon.csv.gz"))
save_to <- "makeuse_coefficients.csv"

# get_runtime_parameters
propertiesFN <- "../tests/default.properties"
format <- "java"
delimiter <- '='
import_flag <- '&'
echo_parameters <- TRUE
save_to <- "ct_runtime_parameters.RData"

# read_skim_matrices
distance_skimsFN <- "../tests/pkautodist.zmx"
time_skimsFN <- "../tests/pkautotime.zmx"
save_to <- "skim_matrices.csv"

# sample_daily_faf_truckloads
annual_faf_trucks <- readr::read_csv("annual_faf_truckloads.csv")
external_scaling_factor <- 2.0
weeks_per_year <- 50
target_week <- 24
target_day <- 4
save_to <- "daily_faf_truckloads.csv"

# sample_local_truck_destinations
skim_matrices <- readr::read_csv("skim_matrices.csv")
truck_origins <- readr::read_csv("daily_local_origins.csv")
save_to <- "daily_local_trips.csv"
utility_parameters <- readr::read_csv(file.path(DATA-RAW, "ct-destination-utility-parameters.csv"))
trip_length_targets <- readr::read_csv(file.path(DATA-RAW, "ct-trip-length-targets.csv"))
