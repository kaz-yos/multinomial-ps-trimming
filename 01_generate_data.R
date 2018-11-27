#!/usr/local/bin/Rscript

################################################################################
### Generate simulation data
##
## Created on: 2017-12-11
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
library(datagen3)


cat("
###
### Load configurations
################################################################################\n")

base_alphas1 <- log(c(2.0, 1.0, 0.2, 1.5, 1.0, 0.5))
base_alphas2 <- -log(c(2.0, 1.0, 0.2, 1.5, 1.0, 0.5))
base_betas <- log(c(1.0, 2.0, 0.2, 1.0, 1.5, 0.5))


cat("
###
### Generate scenarios
################################################################################\n")

lst_lst_possible_values <-
    list(n = list("n=6000" = 6000),
         ## Make them stay together.
         lst_alphas_prev_params_contra =
             c(list("33:33:33; Strong effect on treatment" = list(alphas = c(-0.2, base_alphas1, +10, -10, +3,
                                                                             -0.5, base_alphas2, +10, +2, -10),
                                                                  prev_params = c(0.37, 0.63, 0.70),
                                                                  contraindication = TRUE),
                    "10:45:45; Strong effect on treatment" = list(alphas = c(+1.25, base_alphas1, +10, -10, +2,
                                                                             +0.95, base_alphas2, +10, +2, -10),
                                                                  prev_params = c(0.11, 0.80, 0.85),
                                                                  contraindication = TRUE),
                    "10:10:80; Strong effect on treatment" = list(alphas = c(-0.7, base_alphas1, +10, -10, +2,
                                                                             +2.1, base_alphas2, +10, +2, -10),
                                                                  prev_params = c(0.13, 0.35, 0.92),
                                                                  contraindication = TRUE)),
               list("33:33:33; No effect on treatment" = list(alphas = c(-0.2, base_alphas1, 0, 0, 0,
                                                                         -0.5, base_alphas2, 0, 0, 0),
                                                              prev_params = c(0.37, 0.63, 0.70),
                                                              contraindication = TRUE),
                    "10:45:45; No effect on treatment" = list(alphas = c(+1.25, base_alphas1, 0, 0, 0,
                                                                         +0.95, base_alphas2, 0, 0, 0),
                                                              prev_params = c(0.11, 0.80, 0.85),
                                                              contraindication = TRUE),
                    "10:10:80; No effect on treatment" = list(alphas = c(-0.7, base_alphas1, 0, 0, 0,
                                                                         +2.1, base_alphas2, 0, 0, 0),
                                                              prev_params = c(0.13, 0.35, 0.92),
                                                              contraindication = TRUE))),
         beta0 = list("Rare outcome" = log(0.05),
                      "Common outcome" = log(0.20)),
         betaA = list("No main effect" = c(0,0),
                      "Protective main effect" = c(log(0.9),log(0.6))),
         betaX = list("No confounding" = c(0 * base_betas, 0, 0, 0),
                      "No unmeasured confounding" = c(base_betas, 0, 0, 0),
                      "Moderate unmeasured confounding" = c(base_betas, log(2), log(2), log(2)),
                      "Strong unmeasured confounding" = c(base_betas, log(10), log(10), log(10))),
         betaXA = list("No modification" = c(0,0,0,0,0,0,0,0,0,
                                             0,0,0,0,0,0,0,0,0),
                       "Protective modification by X4" = c(0,0,0,log(0.7),0,0,0,0,0,
                                                           0,0,0,log(0.5),0,0,0,0,0)))

## Split alphas, prev_params, and contraindication
scenarios <- generate_scenario_data_frame(lst_lst_possible_values) %>%
    mutate(alphas = map(lst_alphas_prev_params_contra, magrittr::extract2, "alphas"),
           prev_params = map(lst_alphas_prev_params_contra, magrittr::extract2, "prev_params"),
           contraindication = map(lst_alphas_prev_params_contra, magrittr::extract2, "contraindication")) %>%
    select(n, alphas, prev_params, contraindication, beta0, betaA, betaX, betaXA, description)


## Put all 48 scenarios with no confounding (all beta_X zeros) first
scenarios <- bind_rows(scenarios %>%
                       filter(grepl("No confounding", description)),
                       scenarios %>%
                       filter(!grepl("No confounding", description)))

class(scenarios) <- c("scenarios", class(scenarios))

scenarios
scenarios$description


cat("
###
### Generate datasets
################################################################################\n")

n_parts <- 10
R <- 50

datagen3::generate_data_for_all_scenarios(fun = generate_sturmer_data_count,
                                          scenarios = scenarios,
                                          n_parts = n_parts,
                                          R = R,
                                          ## Do not skip any row
                                          skip = 0,
                                          path = "./data/")


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
