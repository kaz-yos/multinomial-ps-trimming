#!/usr/bin/env ./Rscriptee
## Use the project-specific ./Rscriptee

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

## Check it is a scenario file
if (!grepl("all_analysis_summary", data_file_name)) {
    stop("Not a summary result file")
}


###
### Prepare environment
################################################################################

## When running non-interactively
.script_name. <- gsub("^--file=", "", Filter(function(x) {grepl("^--file=", x)}, commandArgs()))
if (length(.script_name.) == 1) {
    cat("### Running:", paste(commandArgs()), "\n")
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
library(grid)
library(gtable)



cat("
###
### Define functions
################################################################################\n")

## Grid label manipulation
## https://stackoverflow.com/questions/40732543/seeking-workaround-for-gtable-add-grob-code-broken-by-ggplot-2-2-0/40838827#40838827
## Need to overwrite the strip. Thus, make strip background white.
clean_contrast_trim_method_columns <- function(gg) {
    ## Generate a ggplot2 plot grob.
    pg <- ggplotGrob(gg)
    ## Get a list of strips from the original plot
    strip <- lapply(grep("strip-t", pg$layout$name), function(x) {pg$grobs[[x]]})
    ## Construct gtable to contain the new strip
    newStrip <- gtable(widths = unit(rep(1, length(strip)), "null"), heights = strip[[1]]$heights)
    ## Top row
    cols <- seq(1, by = 3, length.out = length(strip)/3)
    newStrip <- gtable_add_grob(newStrip,
                                lapply(strip[cols], `[`, 1),
                                t = 1, l = cols, r = cols + 2)
    ## Bottom row
    newStrip <- gtable_add_grob(newStrip,
                                lapply(strip, `[`, 2),
                                t = 2, l = seq_along(strip))
    ## Put the strip into the plot
    pgNew <- gtable_add_grob(pg, newStrip, t = 6, l = 4, r = 20)
    ## Vertical lines
    pgNew <- gtable_add_grob(x = pgNew,
                             grobs = segmentsGrob(x0 = unit(0.5, "npc"), x1 = unit(0.5, "npc"),
                                                  y0 = unit(0, "npc"), y1 = unit(1, "npc"),
                                                  gp = gpar(lty = 2)),
                             t = 6, l = 9, b = 17, r = 9)
    pgNew <- gtable_add_grob(x = pgNew,
                             grobs = segmentsGrob(x0 = unit(0.5, "npc"), x1 = unit(0.5, "npc"),
                                                  y0 = unit(0, "npc"), y1 = unit(1, "npc"),
                                                  gp = gpar(lty = 2)),
                             t = 6, l = 15, b = 17, r = 15)
    pgNew
}

cat("
###
### Load summary
################################################################################\n")

cat("###  Loading", data_file_name, "\n")
load(data_file_name)

pryr::object_size(df_scenarios)
df_scenarios %>%
    rename(`#` = scenario_count) %>%
    print(n = Inf, width = Inf)

pryr::object_size(df_summary)
df_summary %>%
    print(n = 20)


cat("
###
### Split description for easier use
################################################################################\n")

df_summary <- df_summary %>%
    separate(col = scenario_description, into = c("n","prevalence","incidence","main","unmeasured","modification"), sep = ";") %>%
    mutate(n = as.numeric(gsub("n=","",n)),
           prevalence = factor(prevalence,
                               levels = c("33:33:33","10:45:45","10:10:80")),
           incidence = factor(gsub(" outcome$", "", incidence),
                              levels = c("Rare","Common")),
           main = factor(gsub(" main effect$", "", main),
                         levels = c("No", "Protective")),
           unmeasured = factor(gsub(" unmeasured confounding$", "", unmeasured),
                               levels = c("No","Moderate","Strong")),
           modification = factor(gsub(" modification.*", "", modification),
                                 levels = c("No", "Protective"))) %>%
    ## Ordering only
    mutate(adjustment = factor(adjustment,
                               levels = c("unadj","iptw","mw","ow"),
                               labels = c("Unadj","IPTW","MW","OW")))


cat("
###
### Make no trimming contribute as Crump 0, Sturmer 0, Walker 0
################################################################################\n")

df_summary <-
    bind_rows(df_summary %>% filter(trim_method_name != "none"),
              df_summary %>% filter(trim_method_name == "none") %>% mutate(trim_method_name = "crump"),
              df_summary %>% filter(trim_method_name == "none") %>% mutate(trim_method_name = "sturmer"),
              df_summary %>% filter(trim_method_name == "none") %>% mutate(trim_method_name = "walker")) %>%
    mutate(trim_method_name = factor(trim_method_name,
                                     levels = c("crump","sturmer","walker"),
                                     labels = c("Crump","Sturmer","Walker")))

## For rectangle
## https://stackoverflow.com/questions/9847559/conditionally-change-panel-background-with-facet-grid
df_trim_method_name <- data_frame(trim_method_name = c("Crump","Sturmer","Walker"))

geom_rect_trim <- geom_rect(data = df_trim_method_name,
                            mapping = aes(fill = trim_method_name,
                                          x = NULL, y = NULL, color = NULL, shape = NULL, group = NULL),
                            xmin = -Inf,
                            xmax = Inf,
                            ymin = -Inf,
                            ymax = Inf,
                            alpha = 0/100)


cat("
###
### Sanity check results for the correctness of true results
################################################################################\n")

cat("###  Examine scenario 1 results \n")
df_scenarios %>%
    filter(scenario_count  == 1)

cat("###   No trimming (use Crump 0) \n")
df_summary %>%
    filter(scenario_count  == 1,
           trim_method_name == "Crump",
           thres == 0,
           measure == "coef") %>%
    mutate(mean = round(mean, 3),
           var = round(var, 3)) %>%
    print(n = 100)


cat("###  Examine scenario 4 results \n")
df_scenarios %>%
    filter(scenario_count  == 4)

cat("###   No trimming \n")
df_summary %>%
    filter(scenario_count  == 4,
           trim_method_name == "Crump",
           thres == 0,
           measure == "coef") %>%
    mutate(mean = round(mean, 3),
           var = round(var, 3)) %>%
    print(n = 100)


cat("
###
### Simplify data by restriction to some scenarios
################################################################################\n")

cat('###  filter(main == "Protective", modification == "No", incidence == "Common")\n')
df_summary <- df_summary %>%
    filter(main == "Protective", modification == "No", incidence == "Common")

cat("###  " , length(unique(df_summary$scenario_count)), "scenarios\n")


cat("
###
### Sample size calculation for the full cohort
################################################################################\n")

df_n <- df_summary %>%
    ## remove redundancies
    filter(measure == "coef",
           contrast == "1vs0",
           adjustment == "Unadj",
           !true,
           !reest)

df_n_group <- df_n %>%
    gather(key = group, value = mean_n_group_kept, mean_n0_kept, mean_n1_kept, mean_n2_kept) %>%
    mutate(group = group %>%
               gsub(pattern = "mean_n", replacement = "") %>%
               gsub(pattern = "_kept", replacement = ""))

## After checking sample sizes, restrict to meaningful ones only
cat("###  Restrict to total size n * 1/10\n")
df_summary <- df_summary %>%
    filter(mean_n_kept > n * 1/10)

## Create threshold dataset
thres_crump   <- 1/3 * 1/5
thres_sturmer <- 1/3 * 1/10
thres_walker  <- 1/3 * 3/5
trim_method_name_strings <- c("Crump", "Sturmer", "Walker")
tentative_thres_data <- data_frame(trim_method_name = factor(trim_method_name_strings,
                                                             levels = trim_method_name_strings),
                                   tentative_thres = c(thres_crump, thres_sturmer, thres_walker))


cat("
###
### Bias calculation for mean coef (compare to corresponding true value)
################################################################################\n")

df_bias <- df_summary %>%
    filter(measure == "coef") %>%
    select(-var, -sd) %>%
    spread(key = true, value = mean) %>%
    ## mean of estimates - mean of calculated truth
    mutate(bias = `FALSE` - `TRUE`) %>%
    ## Drop these variables
    select(-`FALSE`, -`TRUE`)


cat("
###
### MSE calculation for coef
################################################################################\n")

df_var <- df_summary %>%
    filter(measure == "coef",
           !true) %>%
    select(-mean)

assertthat::assert_that(nrow(df_bias) == nrow(df_var))

df_mse <- inner_join(df_bias, df_var) %>%
    mutate(mse = var + bias^2)


cat("
###
### Preliminary plotting
################################################################################\n")

## AJE Figure 2
scale <- 0.7
pdf(file = "./out/Figure 2.pdf", width = 8*scale, height = 4*scale, family = "sans")
plot_obj <- df_n %>%
    filter(unmeasured == "No") %>%
    group_by(main, modification, incidence, trim_method_name) %>%
    nest() %>%
    left_join(tentative_thres_data) %>%
    mutate(title_string = c("A","B","C"),
           gg = pmap(list(data, tentative_thres),
                    function(data, tentative_thres) {
                        ggplot(data = data,
                               mapping = aes(x = thres, y = mean_n_kept,
                                             linetype = prevalence, shape = prevalence,
                                             group = interaction(scenario_count, contrast))) +
                            geom_vline(mapping = aes(xintercept = tentative_thres),
                                       size = 0.2, linetype = 2) +
                            geom_line() +
                            geom_point() +
                            scale_y_continuous(breaks = c(0,2000,4000,6000),
                                               labels = c("0", "2,000", "4,000", "6,000")) +
                            scale_shape(guide = FALSE) +
                            scale_linetype(guide = FALSE) +
                            labs(x = "Threshold", y = "Trimmed Sample Size", shape = "Prevalence", linetype = "Prevalence") +
                            theme_bw() +
                            theme(axis.title       = element_text(color = "black", size = 10),
                                  axis.text        = element_text(color = "black", size = 10),
                                  axis.text.x      = element_text(angle = 0, vjust = 0.5, color = "black"),
                                  axis.line        = element_line(),
                                  panel.grid       = element_blank(),
                                  panel.border     = element_blank(),
                                  legend.title     = element_text(size = 10),
                                  legend.key       = element_blank(),
                                  legend.position  = c(0.8, 0.2),
                                  plot.title       = element_text(hjust = 0.5),
                                  plot.background  = element_blank(),
                                  strip.text       = element_blank(),
                                  strip.background = element_blank())
                    }))
do.call(gridExtra::grid.arrange, c(plot_obj$gg, ncol = 3))
dev.off()

## AJE Figure 2 individual panels
plot_obj %>%
    mutate(file_name = pmap(list(title_string, gg), function(label, ggplot) {
        file_name <- sprintf("./out/Figure 2%s.pdf", label)
        ## https://stackoverflow.com/questions/5142842/export-a-graph-to-eps-file-with-r
        ggsave(file = file_name,
               plot = ggplot,
               width = 8*scale/3,
               height = 4*scale)
        file_name
    }))


pdf(file = "./out/mean_n_kept.pdf", width = 8, height = 4, family = "sans")

plot_obj <- df_n %>%
    filter(unmeasured == "No") %>%
    group_by(main, modification, incidence) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence",
                                  main, modification, incidence),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = mean_n_kept,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, contrast))) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   facet_grid(. ~ trim_method_name, scales = "free_x") +
                   labs(x = "Threshold", y = "Mean trimmed sample size", shape = "Prevalence", linetype = "Prevalence") +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
           }))
plot_obj$gg

plot_obj <- df_n_group %>%
    filter(unmeasured == "No") %>%
    group_by(main, modification, incidence) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence",
                                  main, modification, incidence),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = mean_n_group_kept,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, contrast))) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   facet_grid(. ~ group + trim_method_name, scales = "free_x") +
                   labs(title = title_string, x = "Threshold", linetype = "Prevalence", shape = "Prevalence") +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
           }))
plot_obj$gg

dev.off()


pdf(file = "./out/mean_coef.pdf", width = 12, height = 9, family = "sans")

plot_obj <- df_summary %>%
    filter(measure == "coef",
           !reest,
           !true) %>%
    group_by(main, modification, incidence, unmeasured) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence; %s unmeasured confounding",
                                  main, modification, incidence, unmeasured),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = mean,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, contrast))) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   facet_grid(adjustment ~ contrast + trim_method_name, scales = "free_x") +
                   labs(title = title_string, x = "Threshold", linetype = "Prevalence", shape = "Prevalence") +
                   coord_cartesian(ylim = 0 + 1 * c(-1,+1)) +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
                     }),
           pg = map(gg, clean_contrast_trim_method_columns))
for (i in seq_along(plot_obj$pg)) {
    if (i > 1) {grid.newpage()}
    grid.draw(plot_obj$pg[[i]])
}

dev.off()


pdf(file = "./out/mean_coef_true.pdf", width = 12, height = 9, family = "sans")

plot_obj <- df_summary %>%
    filter(measure == "coef",
           !reest,
           true) %>%
    group_by(main, modification, incidence, unmeasured) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence; %s unmeasured confounding",
                                  main, modification, incidence, unmeasured),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = mean,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, contrast))) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   facet_grid(adjustment ~ contrast + trim_method_name, scales = "free_x") +
                   labs(title = title_string, x = "Threshold", linetype = "Prevalence", shape = "Prevalence") +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
                     }),
           pg = map(gg, clean_contrast_trim_method_columns))
for (i in seq_along(plot_obj$pg)) {
    if (i > 1) {grid.newpage()}
    grid.draw(plot_obj$pg[[i]])
}

dev.off()


pdf(file = "./out/mean_coef_true_overlap.pdf", width = 12, height = 9, family = "sans")

plot_obj <- df_summary %>%
    filter(measure == "coef",
           !reest) %>%
    mutate(alpha = as.numeric(!true)) %>%
    group_by(main, modification, incidence, unmeasured) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence; %s unmeasured confounding",
                                  main, modification, incidence, unmeasured),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = mean,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, true, contrast),
                                    alpha = alpha)) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   scale_alpha(guide = FALSE, range = c(0.3, 1.0)) +
                   facet_grid(adjustment ~ contrast + trim_method_name, scales = "free_x") +
                   labs(title = title_string, x = "Threshold", linetype = "Prevalence", shape = "Prevalence") +
                   ## coord_cartesian(ylim = 0 + 1 * c(-1,+1)) +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
                     }),
           pg = map(gg, clean_contrast_trim_method_columns))
for (i in seq_along(plot_obj$pg)) {
    if (i > 1) {grid.newpage()}
    grid.draw(plot_obj$pg[[i]])
}

dev.off()


pdf(file = "./out/bias.pdf", width = 12, height = 9, family = "sans")

plot_obj <- df_bias %>%
    filter(measure == "coef",
           !reest) %>%
    group_by(main, modification, incidence, unmeasured) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence; %s unmeasured confounding",
                                  main, modification, incidence, unmeasured),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = bias,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, contrast))) +
                   geom_hline(yintercept = 0, size = 0.5) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   ## scale_y_continuous(limit = 0 + 2.0 * c(-1,+1)) +
                   facet_grid(adjustment ~ contrast + trim_method_name, scales = "free_x") +
                   labs(title = title_string, x = "Threshold", y = "Bias", linetype = "Prevalence", shape = "Prevalence") +
                   ## coord_cartesian(ylim = 0 + 1 * c(-1,+1)) +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
                     }),
           pg = map(gg, clean_contrast_trim_method_columns))
for (i in seq_along(plot_obj$pg)) {
    if (i > 1) {grid.newpage()}
    grid.draw(plot_obj$pg[[i]])
}

dev.off()


pdf(file = "./out/var_coef.pdf", width = 12, height = 9, family = "sans")

plot_obj <- df_summary %>%
    filter(measure == "coef",
           !reest,
           !true) %>%
    group_by(main, modification, incidence, unmeasured) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence; %s unmeasured confounding",
                                  main, modification, incidence, unmeasured),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = var,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, contrast))) +
                   geom_hline(yintercept = 0, size = 0.5) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   facet_grid(adjustment ~ contrast + trim_method_name, scales = "free_x") +
                   labs(title = title_string, x = "Threshold", linetype = "Prevalence", shape = "Prevalence") +
                   coord_cartesian(ylim = c(0, 0.25)) +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
                     }),
           pg = map(gg, clean_contrast_trim_method_columns))
for (i in seq_along(plot_obj$pg)) {
    if (i > 1) {grid.newpage()}
    grid.draw(plot_obj$pg[[i]])
}

dev.off()


pdf(file = "./out/sd_coef.pdf", width = 12, height = 9, family = "sans")

plot_obj <- df_summary %>%
    filter(measure == "coef",
           !reest,
           !true) %>%
    group_by(main, modification, incidence, unmeasured) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence; %s unmeasured confounding",
                                  main, modification, incidence, unmeasured),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = sd,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, contrast))) +
                   geom_hline(yintercept = 0, size = 0.5) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   facet_grid(adjustment ~ contrast + trim_method_name, scales = "free_x") +
                   labs(title = title_string, x = "Threshold", y = "Simulation standard error", linetype = "Prevalence", shape = "Prevalence") +
                   ## coord_cartesian(ylim = c(0, 0.5)) +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
                     }),
           pg = map(gg, clean_contrast_trim_method_columns))
for (i in seq_along(plot_obj$pg)) {
    if (i > 1) {grid.newpage()}
    grid.draw(plot_obj$pg[[i]])
}

dev.off()


pdf(file = "./out/mse.pdf", width = 12, height = 9, family = "sans")

plot_obj <- df_mse %>%
    filter(measure == "coef",
           !reest) %>%
    group_by(main, modification, incidence, unmeasured) %>%
    nest() %>%
    mutate(title_string = sprintf("%s effect; %s modification; %s incidence; %s unmeasured confounding",
                                  main, modification, incidence, unmeasured),
           gg = map2(data, title_string,
                     function(data, title_string) {
               ggplot(data = data,
                      mapping = aes(x = thres, y = mse,
                                    linetype = prevalence, shape = prevalence,
                                    group = interaction(scenario_count, contrast))) +
                   geom_hline(yintercept = 0, size = 0.5) +
                   geom_vline(data = tentative_thres_data,
                              mapping = aes(xintercept = tentative_thres),
                              size = 0.1) +
                   geom_line() +
                   geom_point() +
                   ## scale_y_continuous(limit = 0 + 2.0 * c(0,+1)) +
                   facet_grid(adjustment ~ contrast + trim_method_name, scales = "free_x") +
                   labs(title = title_string, x = "Threshold", y = "MSE", linetype = "Prevalence", shape = "Prevalence") +
                   ## coord_cartesian(ylim = c(0, 0.75)) +
                   theme_bw() +
                   theme(legend.key = element_blank(),
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(angle = 90, vjust = 0.5),
                         strip.background = element_rect(fill = "white", color = "white"))
                     }),
           pg = map(gg, clean_contrast_trim_method_columns))
for (i in seq_along(plot_obj$pg)) {
    if (i > 1) {grid.newpage()}
    grid.draw(plot_obj$pg[[i]])
}

dev.off()



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
