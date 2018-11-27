#!/usr/local/bin/Rscript

################################################################################
### Analyze trimmed data
##
## Created on: 2017-12-15
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
if (!grepl("scenario_prepared", data_file_name)) {
    stop("Not a prepared scenario file")
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


cat("
###
### Load data file
################################################################################\n")

## Load
load(data_file_name)

cat("### data_file_name\n", data_file_name, "\n")
cat("### scenario_count\n", scenario_count, "\n")
cat("### scenario_description\n", scenario_description, "\n")
cat("### part_count\n", part_count, "\n")
cat("### R\n", R, "\n")


cat("
###
### Analyze data
################################################################################\n")

## Prepare data for analysis readiness
lst_iter <- foreach::foreach(i = seq_along(lst_iter)) %dorng% {
    cat("### Iteration", i, "\n")

    ## Data analysis step
    nested_df <- lst_iter[[i]] %>%
        group_by(trim_method_name, thres) %>%
        mutate(
            ## Record number kept after trimming
            n_kept = as.numeric(map(trimmed_data, nrow)),
            ## List column holding tables
            n_kept_group = map(trimmed_data, function(df) {
                table(df$A)
            }),
            ## Extract group sizes
            n0_kept = map_dbl(n_kept_group, magrittr::extract, 1),
            n1_kept = map_dbl(n_kept_group, magrittr::extract, 2),
            n2_kept = map_dbl(n_kept_group, magrittr::extract, 3),
            ## Analyze and return coef/vcov only
            trimmed_data = map(trimmed_data, function(data) {

                ## Create an augmented counterfactual dataset.
                data_aug <- augment_counterfactuals(data = data,
                                                    outcome_name = "Y",
                                                    counter_names = c("pYA0","pYA1","pYA2"),
                                                    A_name = "A",
                                                    A_levels = c(0,1,2))

                df_outcome <- analyze_outcome_glm(data = data,
                                                  formula = Y ~ factor(A),
                                                  family = poisson(link = "log"),
                                                  data_aug = data_aug)

                df_outcome
            })) %>%
    ## Drop list column holding tables
    select(-n_kept_group)

    nested_df
}


cat("
###
### Save data
################################################################################\n")

## New file name
new_data_file_name <- gsub("prepared", "analyzed", data_file_name)
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
cat("\n### Started  ", as.character(start_time), "\n")
cat("### Finished ", as.character(end_time), "\n")
print(end_time - start_time)
## Stop sinking to a file if active
if (sink.number() != 0) {sink()}
