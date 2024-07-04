DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")

library(gips)

F_call <- 10000

load(file.path(DATADIR, "all_experiments.rda")) # all_experiments
load(file.path(DATADIR, "df_random_perm_log_posteriori.rda")) # df_random_perm_log_posteriori
load(file.path(DATADIR, paste0("results_MH_", F_call, ".rda"))) # MH_list_results
load(file.path(DATADIR, paste0("results_RAND_", F_call, ".rda"))) # RAND_list_results

source(file.path(DATADIR, "..", "00-utils.R"))


get_experiment_result <- function(i) {
  mean_result_MH <- numeric(F_call)
  mean_result_RAND <- numeric(F_call)

  for (j in 1:M) {
    MH_values <- attr(MH_list_results[[i]][[j]], "optimization_info")$log_posteriori_values
    RAND_values <- attr(RAND_list_results[[i]][[j]], "optimization_info")$log_posteriori_values

    value_1 <- max(MH_values, RAND_values)
    value_0 <- min(MH_values, RAND_values)

    mean_result_MH <- mean_result_MH + scale_data(cummax(MH_values), value_0, value_1)/M
    mean_result_RAND <- mean_result_RAND + scale_data(cummax(RAND_values), value_0, value_1)/M
  }

  list(
    "mean_result_MH" = mean_result_MH,
    "mean_result_RAND" = mean_result_RAND
  )
}



i <- 1

restul_list <- get_experiment_result(i)

plot(restul_list$mean_result_MH, type="l", log = "x", ylim = c(0, 1))
lines(restul_list$mean_result_RAND, type="l", col = "blue")

all_experiments[[i]]
i <- i+1

