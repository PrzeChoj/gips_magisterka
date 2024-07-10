DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")

library(gips)

F_call <- 10000

data_amount <- 12
my_data <- list()
data_name <- c()

load(file.path(DATADIR, "all_experiments.rda")) # all_experiments
load(file.path(DATADIR, "df_random_perm_log_posteriori.rda")) # df_random_perm_log_posteriori

load(file.path(DATADIR, paste0("results_MH_", F_call, ".rda"))) # MH_list_results
my_data[[length(my_data) + 1]] <- MH_list_results
data_name[length(data_name) + 1] <- "MH"
load(file.path(DATADIR, paste0("results_RAND_", F_call, ".rda"))) # RAND_list_results
my_data[[length(my_data) + 1]] <- RAND_list_results
data_name[length(data_name) + 1] <- "RAND"
load(file.path(DATADIR, paste0("results_SA_0_3_", F_call, ".rda"))) # SA_list_results
my_data[[length(my_data) + 1]] <- SA_list_results
data_name[length(data_name) + 1] <- "SA_0_3"
load(file.path(DATADIR, paste0("results_SA_0_1_", F_call, ".rda"))) # SA_list_results
my_data[[length(my_data) + 1]] <- SA_list_results
data_name[length(data_name) + 1] <- "SA_0_1"
load(file.path(DATADIR, paste0("results_SA_0_01_", F_call, ".rda"))) # SA_list_results
my_data[[length(my_data) + 1]] <- SA_list_results
data_name[length(data_name) + 1] <- "SA_0_01"
load(file.path(DATADIR, paste0("results_MH_S_Const_0_9_", F_call, ".rda"))) # MH_S_list_results
my_data[[length(my_data) + 1]] <- MH_S_list_results
data_name[length(data_name) + 1] <- "MH_S_Const_0_9"
load(file.path(DATADIR, paste0("results_MH_S_lin_", F_call, ".rda"))) # MH_S_list_results
my_data[[length(my_data) + 1]] <- MH_S_list_results
data_name[length(data_name) + 1] <- "MH_S_lin"
load(file.path(DATADIR, paste0("results_MH_S_cos_", F_call, ".rda"))) # MH_S_list_results
my_data[[length(my_data) + 1]] <- MH_S_list_results
data_name[length(data_name) + 1] <- "MH_S_cos"
load(file.path(DATADIR, paste0("results_MH_S_logcos_", F_call, ".rda"))) # MH_S_list_results
my_data[[length(my_data) + 1]] <- MH_S_list_results
data_name[length(data_name) + 1] <- "MH_S_logcos"
load(file.path(DATADIR, paste0("results_MH_sq_a0_1_b0_1_F", F_call, ".rda"))) # MH_sq_list_results
my_data[[length(my_data) + 1]] <- MH_sq_list_results
data_name[length(data_name) + 1] <- "MH_sq_unequal"
load(file.path(DATADIR, paste0("results_MH_sq_a0_2_b0_5_F", F_call, ".rda"))) # MH_sq_list_results
my_data[[length(my_data) + 1]] <- MH_sq_list_results
data_name[length(data_name) + 1] <- "MH_sq_equal"
load(file.path(DATADIR, paste0("results_MH_sq_a0_9_b0_5_F", F_call, ".rda"))) # MH_sq_list_results
my_data[[length(my_data) + 1]] <- MH_sq_list_results
data_name[length(data_name) + 1] <- "MH_sq_loads"

stopifnot(length(my_data) == data_amount)
stopifnot(length(data_name) == data_amount)


source(file.path(DATADIR, "..", "00-utils.R"))

get_experiment_result <- function(i) {
  this_result_length <- data_amount * 2

  my_results <- vector('list', this_result_length)

  for (k in 1:data_amount) {
    my_results[[2*k - 1]] <- numeric(F_call)
    my_results[[2*k]] <- numeric(M)
  }

  for (j in 1:M) {
    my_values <- vector('list', data_amount)
    for (k in 1:data_amount) {
      my_values[[k]] <- attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values
    }

    value_1 <- max(sapply(my_values, max))
    value_0 <- min(sapply(my_values, min))

    my_results
    for (k in 1:data_amount) {
      my_results[[2*k - 1]] <- my_results[[2*k - 1]] + scale_data(cummax(my_values[[k]]), value_0, value_1)/M
      my_results[[2*k]][j] <- max(my_values[[k]])
    }
  }

  names(my_results) <- vector('character', this_result_length)

  for (k in 1:data_amount) {
    names(my_results)[2*k - 1] <- paste0("mean_result_", data_name[k])
    names(my_results)[2*k] <- paste0("max_result_", data_name[k])
  }

  my_results
}

plot_results <- function(type_plot) {
  for (i in 1:length(MH_list_results)) {
    restul_list <- get_experiment_result(i)

    plot(restul_list$mean_result_MH, type = "l", log = "x", ylim = c(0, 1))
    lines(restul_list$mean_result_RAND, type = "l", col = "blue")
    legend_name <- c("MH", "RAND")

    if (type_plot == "SA") {
      lines(restul_list$mean_result_SA_0_3, type = "l", col = "magenta")
      lines(restul_list$mean_result_SA_0_1, type = "l", col = "red")
      lines(restul_list$mean_result_SA_0_01, type = "l", col = "green")
      legend_name <- c(legend_name, "SA 0.3", "SA 0.1", "SA 0.01")
    } else if (type_plot == "MH_S") {
      lines(restul_list$mean_result_MH_S_Const_0_9, type = "l", col = "magenta")
      lines(restul_list$mean_result_MH_S_lin, type = "l", col = "red")
      lines(restul_list$mean_result_MH_S_cos, type = "l", col = "green")
      legend_name <- c(legend_name, "MH_S Const 0.9", "MH_S lin", "MH_S cos")
    } else if (type_plot == "MH_sq") {
      lines(restul_list$mean_result_MH_sq, type = "l", col = "magenta")
      lines(restul_list$mean_result_MH_sq_equal, type = "l", col = "red")
      lines(restul_list$mean_result_MH_sq_loads, type = "l", col = "green")
      legend_name <- c(legend_name, "MH_sq", "MH_sq_equal", "MH_sq_loads")
    }

    legend(
      1, 0.99, legend = legend_name,
      col = c("black", "blue", "magenta", "red", "green"),
      lty = 1, cex = 0.5
    )

    print(paste0("n = ", all_experiments[[i]]$n))
    print(paste0("p = ", attr(all_experiments[[i]]$true_perm, "size")))
    print(paste0("cicle length = ", length(all_experiments[[i]]$true_perm[[1]])))
    readline(prompt="Press [enter] to continue")
  }
}

plot_results("SA")
plot_results("MH_S")
plot_results("MH_sq")
