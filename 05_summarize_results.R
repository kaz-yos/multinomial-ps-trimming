#!/usr/local/bin/Rscript

################################################################################
### Assess aggregated results
##
## Created on: 2017-12-28
## Author: Kazuki Yoshida
################################################################################


###
### Capture data filename argument
################################################################################

## Specify data file as the first argument
data_file_name <- commandArgs(trailingOnly = TRUE)[1]
## Specify the core count as the second argument
n_cores <- as.numeric(commandArgs(trailingOnly = TRUE)[2])

## Execution not allowed without data file
stopifnot(!is.na(data_file_name))
## Execution not allowed without n_cores
stopifnot(!is.na(n_cores))

## Check it is an aggregated result file
if (!grepl("all_analysis_results", data_file_name)) {
    stop("Not an aggregated result file")
}


###
### Prepare environment
################################################################################

## sink() if being run non-interactively
if (sink.number() != 0) {sink()}
.script_name. <- gsub("^--file=", "", Filter(function(x) {grepl("^--file=", x)}, commandArgs()))
if (length(.script_name.) == 1) {
    sink(file = paste0("./log/", .script_name., ".txt"), split = TRUE)
    options(width = 100)
}

## Record start time
start_time <- Sys.time()
cat("### Started ", as.character(start_time), "\n")

## Configure parallelization
## Parallel backend for foreach (also loads foreach and parallel; includes doMC)
library(doParallel)
## Reproducible parallelization
library(doRNG)
## Used by parallel::mclapply() as default
options(mc.cores = n_cores)
## Used by doParallel as default
options(cores = n_cores)
## Register doParallel as the parallel backend for foreach
## http://stackoverflow.com/questions/28989855/the-difference-between-domc-and-doparallel-in-r
doParallel::registerDoParallel(cores = n_cores)
## Report multicore use
cat("### Using", foreach::getDoParWorkers(), "cores\n")
cat("### Using", foreach::getDoParName(), "as backend\n")

## Load packages
library(tidyverse)


cat("
###
### Load aggregated results
################################################################################\n")

load(data_file_name)

pryr::object_size(df_scenarios)
df_scenarios
pryr::object_size(df_results)
df_results %>%
    select(-scenario_description)


cat("
###
### Show all scenarios
################################################################################\n")

df_scenarios %>%
    select(-scenario) %>%
    rename(`#` = scenario_count) %>%
    print(n = Inf, width = Inf)


cat("
###
### Summarize to mean and variance
################################################################################\n")

df_summary <- df_results %>%
    group_by(scenario_count, scenario_description, trim_method_name, thres, adjustment, measure, contrast) %>%
    ## At most, means and variances are what we need.
    summarize(mean = mean(value, na.rm = TRUE),
              var = var(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE),
              mean_n_kept = mean(n_kept, na.rm = TRUE),
              mean_n0_kept = mean(n0_kept, na.rm = TRUE),
              mean_n1_kept = mean(n1_kept, na.rm = TRUE),
              mean_n2_kept = mean(n2_kept, na.rm = TRUE),
              min_n_kept = min(n_kept, na.rm = TRUE),
              min_n0_kept = min(n0_kept, na.rm = TRUE),
              min_n1_kept = min(n1_kept, na.rm = TRUE),
              min_n2_kept = min(n2_kept, na.rm = TRUE),
              max_n_kept = max(n_kept, na.rm = TRUE),
              max_n0_kept = max(n0_kept, na.rm = TRUE),
              max_n1_kept = max(n1_kept, na.rm = TRUE),
              max_n2_kept = max(n2_kept, na.rm = TRUE)) %>%
    ## Move scenario_description to the last
    select(everything(), -scenario_description, scenario_description)

cat("###  Check size\n")
pryr::object_size(df_summary)
df_summary %>%
    ungroup() %>%
    select(-scenario_description)

## Move scenario_description to the last
df_summary <- df_summary %>%
    select(everything(), -scenario_description, scenario_description)

cat("###  Mark true values\n")
df_summary <- df_summary %>%
    ungroup() %>%
    mutate(true = grepl("_t$", adjustment),
           adjustment = gsub("_t$", "", adjustment)) %>%
    select(scenario_count, trim_method_name, thres, adjustment, true, everything())
df_summary

cat("###  Mark PS re-estimation in trimmed cohort\n")
df_summary <- df_summary %>%
    ungroup() %>%
    mutate(reest = grepl("2$", adjustment),
           adjustment = gsub("1$|2$", "", adjustment)) %>%
    select(scenario_count, trim_method_name, thres, adjustment, reest, true, everything())
df_summary


cat("
###
### Save summarized results only
################################################################################\n")

save(df_summary,
     df_scenarios,
     file = gsub("results", "summary", data_file_name))


################################################################################
cat("
###
### Record package versions etc
################################################################################\n")
print(sessionInfo())
## Record execution time
end_time <- Sys.time()
cat("\n### Started  ", as.character(start_time), "\n")
cat("### Finished ", as.character(end_time), "\n")
print(end_time - start_time)
## Stop sinking to a file if active
if (sink.number() != 0) {sink()}
