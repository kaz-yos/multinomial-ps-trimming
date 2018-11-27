#!/usr/local/bin/Rscript

################################################################################
### Prepare datasets for further analyses
##
## Created on: 2017-12-11
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

## Check it is a scenario file
if (!grepl("scenario_raw", data_file_name)) {
    stop("Not a raw scenario file")
}


###
### Prepare environment
################################################################################

## sink() if being run non-interactively
if (sink.number() != 0) {sink()}
.scriptFileName. <- gsub("^--file=", "", Filter(function(x) {grepl("^--file=", x)}, commandArgs()))
if (length(.scriptFileName.) == 1) {
    ## Include data file name
    sink(file = paste0("./log/",
                       basename(.scriptFileName.),
                       "_",
                       ## Data file name without folder or extension
                       tools::file_path_sans_ext(basename(data_file_name)),
                       ".txt"), split = TRUE)
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
library(datagen3)
library(trim3)

set.seed(300891730)


cat("
###
### Load data file
################################################################################\n")

## Configure trimming strategies
df_trim_thres <- tribble(
    ~trim_method_name, ~thres,
    "none", 0,
    ##
    "crump", 0.30,
    "crump", 0.25,
    "crump", 0.20,
    "crump", 0.15,
    "crump", 0.10,
    "crump", 1/15,
    "crump", 1/30,
    "crump", 1/60,
    ##
    "sturmer", 0.30,
    "sturmer", 0.25,
    "sturmer", 0.20,
    "sturmer", 0.15,
    "sturmer", 0.10,
    "sturmer", 0.05,
    "sturmer", 1/30,
    "sturmer", 1/60,
    ##
    "walker", 0.30,
    "walker", 0.25,
    "walker", 0.20,
    "walker", 0.15,
    "walker", 0.10,
    "walker", 0.05,
    "walker", 1/40)

## Load
load(data_file_name)

cat("### data_file_name\n", data_file_name, "\n")
cat("### scenario_count\n", scenario_count, "\n")
cat("### scenario_description\n", scenario_description, "\n")
cat("### part_count\n", part_count, "\n")
cat("### R\n", R, "\n")


cat("
###
### Prepare data (Estimate GPS, trim, re-estimate GPS, and add weights)
################################################################################\n")

## Prepare data for analysis readiness
lst_iter <- foreach::foreach(i = seq_along(lst_iter)) %dorng% {
    cat("### Iteration", i, "\n")

    ## Data preparation step
    nested_df <- lst_iter[[i]] %>%
        mutate(A = factor(A, levels = c(0,1,2))) %>%
        prepare_data(data = .,
                     formula1 = A ~ X1 + X2 + X3 + X4 + X5 + X6,
                     formula2 = A ~ X1 + X2 + X3 + X4 + X5 + X6,
                     family = VGAM::multinomial(parallel = FALSE),
                     ps_prefix1 = "ps1_",
                     ps_prefix2 = "ps2_",
                     df_trim_thres = df_trim_thres,
                     A_name = "A",
                     levels = c("0","1","2"))

    nested_df
}


cat("
###
### Save data
################################################################################\n")

## New file name
new_data_file_name <- gsub("raw", "prepared", data_file_name)
cat("###  Saving", new_data_file_name,"\n")

## Save everything as a new file
save(scenario, R, scenario_count, scenario_description, part_count, lst_iter,
     file = new_data_file_name)


################################################################################
cat("
###
### Record package versions etc
################################################################################\n")
print(sessionInfo())
## Record execution time
end_time <- Sys.time()
cat("### Started  ", as.character(start_time), "\n")
cat("### Finished ", as.character(end_time), "\n")
print(end_time - start_time)
## Stop sinking to a file if active
if (sink.number() != 0) {sink()}
