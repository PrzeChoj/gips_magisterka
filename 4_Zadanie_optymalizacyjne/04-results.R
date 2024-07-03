DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")

library(gips)

load(file.path(DATADIR, "all_experiments.rda")) # all_experiments
load(file.path(DATADIR, "df_random_perm_log_posteriori.rda")) # list_random_perm_log_posteriori
load(file.path(DATADIR, "results_MH_10000.rda")) # my_MH_list_results

source(file.path(DATADIR, "..", "00-utils.R"))



plot(my_MH_list_results[[2]][[3]])
df_random_perm_log_posteriori[[2]][[3]]
this_exp <- all_experiments[[2]]
S <- attr(my_MH_list_results[[2]][[3]], "S")
number_of_observations <- attr(my_MH_list_results[[2]][[3]], "number_of_observations")

gips_best <- gips(
  S, number_of_observations, was_mean_estimated = FALSE, perm = all_experiments[[2]]$true_perm
)

compare_log_posteriories_of_perms(my_MH_list_results[[2]][[3]], gips_best)
