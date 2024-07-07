DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")

plot_SA <- FALSE

library(gips)

F_call <- 10000

load(file.path(DATADIR, "all_experiments.rda")) # all_experiments
load(file.path(DATADIR, "df_random_perm_log_posteriori.rda")) # df_random_perm_log_posteriori
load(file.path(DATADIR, paste0("results_MH_", F_call, ".rda"))) # MH_list_results
load(file.path(DATADIR, paste0("results_RAND_", F_call, ".rda"))) # RAND_list_results
load(file.path(DATADIR, paste0("results_SA_0_3_", F_call, ".rda"))) # SA_list_results
SA_list_results_0_3 <- SA_list_results
load(file.path(DATADIR, paste0("results_SA_0_1_", F_call, ".rda"))) # SA_list_results
SA_list_results_0_1 <- SA_list_results
load(file.path(DATADIR, paste0("results_SA_0_01_", F_call, ".rda"))) # SA_list_results
SA_list_results_0_01 <- SA_list_results
load(file.path(DATADIR, paste0("results_MH_S_Const_0_9_", F_call, ".rda"))) # MH_S_list_results
MH_S_list_results_Const_0_9 <- MH_S_list_results
load(file.path(DATADIR, paste0("results_MH_S_lin_", F_call, ".rda"))) # MH_S_list_results
MH_S_list_results_lin <- MH_S_list_results
load(file.path(DATADIR, paste0("results_MH_S_cos_", F_call, ".rda"))) # MH_S_list_results
MH_S_list_results_cos <- MH_S_list_results
load(file.path(DATADIR, paste0("results_MH_S_logcos_", F_call, ".rda"))) # MH_S_list_results
MH_S_list_results_logcos <- MH_S_list_results

source(file.path(DATADIR, "..", "00-utils.R"))

get_experiment_result <- function(i) {
  mean_result_MH <- numeric(F_call)
  mean_result_RAND <- numeric(F_call)
  mean_result_SA_0_3 <- numeric(F_call)
  mean_result_SA_0_1 <- numeric(F_call)
  mean_result_SA_0_01 <- numeric(F_call)
  mean_result_MH_S_Const_0_9 <- numeric(F_call)
  mean_result_MH_S_lin <- numeric(F_call)
  mean_result_MH_S_cos <- numeric(F_call)
  mean_result_MH_S_logcos <- numeric(F_call)

  for (j in 1:M) {
    MH_values <- attr(MH_list_results[[i]][[j]], "optimization_info")$log_posteriori_values
    RAND_values <- attr(RAND_list_results[[i]][[j]], "optimization_info")$log_posteriori_values
    SA_0_3_values <- attr(SA_list_results_0_3[[i]][[j]], "optimization_info")$log_posteriori_values
    SA_0_1_values <- attr(SA_list_results_0_1[[i]][[j]], "optimization_info")$log_posteriori_values
    SA_0_01_values <- attr(SA_list_results_0_01[[i]][[j]], "optimization_info")$log_posteriori_values
    MH_S_Const_0_9_values <- attr(MH_S_list_results_Const_0_9[[i]][[j]], "optimization_info")$log_posteriori_values
    MH_S_lin_values <- attr(MH_S_list_results_lin[[i]][[j]], "optimization_info")$log_posteriori_values
    MH_S_cos_values <- attr(MH_S_list_results_cos[[i]][[j]], "optimization_info")$log_posteriori_values
    MH_S_logcos_values <- attr(MH_S_list_results_logcos[[i]][[j]], "optimization_info")$log_posteriori_values

    all_values <- c(MH_values, RAND_values, SA_0_3_values, SA_0_1_values, SA_0_01_values, MH_S_Const_0_9_values, MH_S_lin_values, MH_S_cos_values, MH_S_logcos_values)
    value_1 <- max(all_values)
    value_0 <- min(all_values)

    mean_result_MH <- mean_result_MH + scale_data(cummax(MH_values), value_0, value_1)/M
    mean_result_RAND <- mean_result_RAND + scale_data(cummax(RAND_values), value_0, value_1)/M
    mean_result_SA_0_3 <- mean_result_SA_0_3 + scale_data(cummax(SA_0_3_values), value_0, value_1)/M
    mean_result_SA_0_1 <- mean_result_SA_0_1 + scale_data(cummax(SA_0_1_values), value_0, value_1)/M
    mean_result_SA_0_01 <- mean_result_SA_0_01 + scale_data(cummax(SA_0_01_values), value_0, value_1)/M
    mean_result_MH_S_Const_0_9 <- mean_result_MH_S_Const_0_9 + scale_data(cummax(MH_S_Const_0_9_values), value_0, value_1)/M
    mean_result_MH_S_lin <- mean_result_MH_S_lin + scale_data(cummax(MH_S_lin_values), value_0, value_1)/M
    mean_result_MH_S_cos <- mean_result_MH_S_cos + scale_data(cummax(MH_S_cos_values), value_0, value_1)/M
    mean_result_MH_S_logcos <- mean_result_MH_S_logcos + scale_data(cummax(MH_S_logcos_values), value_0, value_1)/M
  }

  list(
    "mean_result_MH" = mean_result_MH,
    "mean_result_RAND" = mean_result_RAND,
    "mean_result_SA_0_3" = mean_result_SA_0_3,
    "mean_result_SA_0_1" = mean_result_SA_0_1,
    "mean_result_SA_0_01" = mean_result_SA_0_01,
    "mean_result_MH_S_Const_0_9" = mean_result_MH_S_Const_0_9,
    "mean_result_MH_S_lin" = mean_result_MH_S_lin,
    "mean_result_MH_S_cos" = mean_result_MH_S_cos,
    "mean_result_MH_S_logcos" = mean_result_MH_S_logcos
  )
}



for (i in 1:length(MH_list_results)) {
  restul_list <- get_experiment_result(i)

  plot(restul_list$mean_result_MH, type="l", log = "x", ylim = c(0, 1))
  lines(restul_list$mean_result_RAND, type="l", col = "blue")
  if (plot_SA) {
    lines(restul_list$mean_result_SA_0_3, type="l", col = "pink")
    lines(restul_list$mean_result_SA_0_1, type="l", col = "red")
    lines(restul_list$mean_result_SA_0_01, type="l", col = "green")
  }
  lines(restul_list$mean_result_MH_S_Const_0_9, type="l", col = "pink")
  lines(restul_list$mean_result_MH_S_lin, type="l", col = "red")
  lines(restul_list$mean_result_MH_S_cos, type="l", col = "green")
  lines(restul_list$mean_result_MH_S_logcos, type="l", col = "magenta")

  legend_name <- c("MH", "RAND")
  if (plot_SA) {
    legend_name <- c(legend_name, "SA 0.3", "SA 0.1", "SA 0.01")
  }
  legend_name <- c(legend_name, "MH_S Const 0.9", "MH_S lin", "MH_S cos", "MH_S logcos")

  legend(
    1, 0.99, legend = legend_name,
    col = c("black", "blue", "pink", "red", "green", "magenta"), lty = 1
  )

  print(paste0("n = ", all_experiments[[i]]$n))
  print(paste0("p = ", attr(all_experiments[[i]]$true_perm, "size")))
  print(paste0("cicle length = ", length(all_experiments[[i]]$true_perm[[1]])))
  readline(prompt="Press [enter] to continue")
}
