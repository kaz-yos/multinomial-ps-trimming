#!/usr/local/bin/Rscript

################################################################################
### Aggregate results
##
## Created on: 2017-12-15
## Author: Kazuki Yoshida
################################################################################


###
### Capture data filename argument
################################################################################

## Specify the core count as the first argument
n_cores <- as.numeric(commandArgs(trailingOnly = TRUE)[1])

## Execution not allowed without n_cores
stopifnot(!is.na(n_cores))


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
library(trim3)


cat("
###
### Load files into a nested data_frame
################################################################################\n")

## All analyzed data files should be contained in this vector.
analyzed_file_names <- dir("./data", "analyzed", full.names = TRUE)

cat("###  Files used\n")
analyzed_file_names

## data_frame with rows representing methods within iteration
df_out <- data_frame(file_name = analyzed_file_names) %>%
    mutate(data = map(file_name, function(file_name) {

        ## Load the analyzed data file for the saved objects.
        cat("###  Loading", file_name, "\n")
        load(file_name)

        ## Organized into a file-level single-row data_frame
        data_frame(scenario_count = scenario_count,
                   scenario = list(scenario),
                   scenario_description = scenario_description,
                   part_count = part_count,
                   results = list(bind_rows(lst_iter) %>% unnest))

    })) %>%
    ## Unnest the filename part
    unnest() %>%
    select(file_name, results, everything())

df_out


cat("
###
### Split into a scenario lookup data_frame and expanded iteration result data_frame
################################################################################\n")

## scenario_description

df_scenarios <- df_out %>%
    select(-file_name, -results, -part_count) %>%
    ## Remove duplicates to reduce it to scenario-level rows (not file-level rows)
    distinct(scenario_count, scenario_description, .keep_all = TRUE) %>%
    select(scenario_count, scenario_description, everything())
cat("### df_scenarios size\n")
pryr::object_size(df_scenarios)
df_scenarios

df_results <- df_out %>%
    select(scenario_count, scenario_description, results) %>%
    unnest()
cat("### df_results size\n")
pryr::object_size(df_results)
df_results


cat("
###
### Calculate the third contrast
################################################################################\n")

cat("
###  Sanity check before transformation\n")
df_results[1,"coef"][[1]]
df_results[1,"vcov"][[1]]

## Create coefficients
## This automatically gives NA when the item is a try-error object.
df_results$coef_A1vs0 <- as.numeric(lapply(df_results$coef, `[`, "factor(A)1"))
df_results$coef_A2vs0 <- as.numeric(lapply(df_results$coef, `[`, "factor(A)2"))
df_results$coef_A2vs1 <- df_results$coef_A2vs0 - df_results$coef_A1vs0
df_results$coef <- NULL

## Create variances
df_results$var_A1vs0 <- as.numeric(lapply(df_results$vcov, function(x) {
    if (is.error(x)) {
        return(as.numeric(NA))
    } else if (dim(x)[1] < 3 | dim(x)[2] < 3) {
        return(as.numeric(NA))
    } else {
        return(x["factor(A)1", "factor(A)1"])
    }
}))
df_results$var_A2vs0 <- as.numeric(lapply(df_results$vcov, function(x) {
    if (is.error(x)) {
        return(as.numeric(NA))
    } else if (dim(x)[1] < 3 | dim(x)[2] < 3) {
        return(as.numeric(NA))
    } else {
        return(x["factor(A)2", "factor(A)2"])
    }
}))
df_results$var_A2vs1 <- as.numeric(lapply(df_results$vcov, function(x) {
    if (is.error(x)) {
        return(as.numeric(NA))
    } else if (dim(x)[1] < 3 | dim(x)[2] < 3) {
        return(as.numeric(NA))
    } else {
        return(x["factor(A)2", "factor(A)2"] + x["factor(A)1", "factor(A)1"] - 2 * x["factor(A)1", "factor(A)2"])
    }
}))
df_results$vcov <- NULL


## Make measures (coef and var) as well as contrast form rows
df_results <- df_results %>%
    gather(key = key, value = value,
           -scenario_count,
           -scenario_description,
           -trim_method_name,
           -thres,
           -n_kept,
           -n0_kept,
           -n1_kept,
           -n2_kept,
           -adjustment) %>%
    ## Separate a character column into two columns by string manipulation.
    separate(key, into = c("measure","contrast"), sep = "_A")

df_results %>%
    select(-scenario_description)


cat("
###
### Save results
################################################################################\n")

save(df_scenarios,
     df_results,
     file = "./data/all_analysis_results.RData")


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
