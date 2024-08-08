library(magrittr)
library(gips)

DATADIR <- file.path(".", "5_Zadanie_optymalizacyjne", "data")

F_call <- 10000

data_amount <- 13
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
load(file.path(DATADIR, paste0("results_SA_10_", F_call, ".rda"))) # SA_list_results
my_data[[length(my_data) + 1]] <- SA_list_results
data_name[length(data_name) + 1] <- "SA_10"
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
load(file.path(DATADIR, paste0("results_EA_Test1_F", F_call, ".rda"))) # EA_list_results
my_data[[length(my_data) + 1]] <- EA_list_results
data_name[length(data_name) + 1] <- "EA_Test1"

stopifnot(length(my_data) == data_amount)
stopifnot(length(data_name) == data_amount)

source(file.path(DATADIR, "..", "00-utils.R"))

# plot 5.2
{
  par(mfrow = c(1, 2))

  i <- 4
  k <- 1 # MH

  min_value <- min(sapply(1:10, function(j){min(attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values)}))
  max_value <- max(sapply(1:10, function(j){max(attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values)}))

  j <- 1
  attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values %>%
    plot(
      log = "x",
      type = "l", ylim = c(min_value, max_value), xlab = "Liczba iteracji", ylab = "Logarytm funkcji celu",
      main = "Oryginalne 10 przebiegów MH na przykładowym zadaniu"
    )
  for (j in 2:10) {
    attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values %>%
      lines(log = "x")
  }

  j <- 1
  attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values %>%
    scale_data(
      min(attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values),
      max(attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values)
    ) %>%
    plot(
      log = "x",
      type = "l", ylim = c(0, 1), xlab = "Liczba iteracji", ylab = "Przeskalowany logarytm funkcji celu",
      main = "Przeskalowane 10 przebiegów MH na przykładowym zadaniu"
    )
  for (j in 2:10) {
    attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values %>%
      scale_data(
        min(attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values),
        max(attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values)
      ) %>%
      lines(log = "x")
  }

  par(mfrow = c(1, 1))
}

get_experiment_result <- function(i) {
  this_result_length <- data_amount * 2 + 1

  my_results <- vector("list", this_result_length)

  for (k in 1:data_amount) {
    my_results[[2*k - 1]] <- numeric(F_call)
    my_results[[2*k]] <- numeric(M)
  }
  my_results[[2*data_amount + 1]] <- numeric(M)


  for (j in 1:M) {
    my_values <- vector("list", data_amount)
    for (k in 1:(data_amount - 1)) { # No evolution
      my_values[[k]] <- attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values
    }

    value_1 <- max(sapply(my_values, max))
    value_0 <- min(sapply(my_values, min))

    for (k in data_amount) { # Add evolution
      my_values[[k]] <- attr(my_data[[k]][[i]][[j]], "optimization_info")$log_posteriori_values
    }

    for (k in 1:data_amount) {
      if (length(my_values[[k]]) < F_call) { # Evolution algorithm can stop a bit early
        my_values[[k]][(length(my_values[[k]]) + 1) : F_call] <- my_values[[k]][1]
      }
      my_results[[2*k - 1]] <- my_results[[2*k - 1]] + scale_data(cummax(my_values[[k]]), value_0, value_1)/M
      my_results[[2*k]][j] <- max(my_values[[k]])
    }
    S <- attr(my_data[[1]][[i]][[j]], "S")
    number_of_observations <- attr(my_data[[1]][[i]][[j]], "number_of_observations")
    g <- gips(
      S, number_of_observations, was_mean_estimated = FALSE,
      perm = all_experiments[[i]]$true_perm
    )
    my_results[[2*data_amount + 1]][j] <- log_posteriori_of_gips(g)
  }

  names(my_results) <- vector("character", this_result_length)

  for (k in 1:data_amount) {
    names(my_results)[2*k - 1] <- paste0("mean_result_", data_name[k])
    names(my_results)[2*k] <- paste0("max_result_", data_name[k])
  }
  names(my_results)[2*data_amount + 1] <- "true_perm_value"

  my_results
}

plot_results <- function(type_plot, focus = FALSE) {
  for (i in 1:length(MH_list_results)) {
    result_list <- get_experiment_result(i)

    if (focus) {
      my_max <- 1
      plot(
        result_list$mean_result_MH, type = "l", log = "x", ylim = c(result_list$mean_result_MH[1000] - 0.02, my_max), xlim = c(1000, 10000),
        main = paste0("Wykresy ECDF przebiegu dla eksperymentu\nn = ", all_experiments[[i]]$n, "; p = ", attr(all_experiments[[i]]$true_perm, "size"), "; długość cyklu = ", length(all_experiments[[i]]$true_perm[[1]])),
        xlab = "Liczba iteracji", ylab = "Przeskalowany logarytm funkcji celu"
      )
    } else {
      plot(
        result_list$mean_result_MH, type = "l", log = "x", ylim = c(0, 1),
        main = paste0("Wykresy ECDF przebiegu dla eksperymentu\nn = ", all_experiments[[i]]$n, "; p = ", attr(all_experiments[[i]]$true_perm, "size"), "; długość cyklu = ", length(all_experiments[[i]]$true_perm[[1]])),
        xlab = "Liczba iteracji", ylab = "Przeskalowany logarytm funkcji celu"
      )
    }

    lines(result_list$mean_result_RAND, type = "l", col = "blue")
    legend_name <- c("MH", "RAND")

    if (type_plot == "SA") {
      lines(result_list$mean_result_SA_10, type = "l", col = "magenta")
      lines(result_list$mean_result_SA_0_1, type = "l", col = "red")
      lines(result_list$mean_result_SA_0_01, type = "l", col = "green")
      legend_name <- c(legend_name, "SA 10", "SA 0.1", "SA 0.01")
    } else if (type_plot == "MH_S") {
      lines(result_list$mean_result_MH_S_Const_0_9, type = "l", col = "magenta")
      lines(result_list$mean_result_MH_S_lin, type = "l", col = "red")
      lines(result_list$mean_result_MH_S_cos, type = "l", col = "green")
      legend_name <- c(legend_name, "MH_S Const 0.9", "MH_S lin", "MH_S cos")
    } else if (type_plot == "MH_sq") {
      lines(result_list$mean_result_MH_sq_unequal, type = "l", col = "magenta")
      lines(result_list$mean_result_MH_sq_equal, type = "l", col = "red")
      lines(result_list$mean_result_MH_sq_loads, type = "l", col = "green")
      legend_name <- c(legend_name, "sq_a_0_1", "sq_a_0_2", "sq_a_0_9")
    } else if (type_plot == "EA") {
      lines(result_list$mean_result_EA_Test1, type = "l", col = "magenta")
      #lines(result_list$mean_result_, type = "l", col = "red")
      #lines(result_list$mean_result_, type = "l", col = "green")
      legend_name <- c(legend_name, "EA")#, "EA2", "EA3")
    }

    legend(
      if(focus){1000}else{1}, 0.99, legend = legend_name,
      col = c("black", "blue", "magenta", "red", "green"),
      lty = 1, cex = 0.8
    )

    ic(i)
    print(paste0("n = ", all_experiments[[i]]$n))
    print(paste0("p = ", attr(all_experiments[[i]]$true_perm, "size")))
    print(paste0("cicle length = ", length(all_experiments[[i]]$true_perm[[1]])))
    readline(prompt="Press [enter] to continue")
  }
}

times_better <- function (exp_1, exp_2) {
  exp_1 <- paste0("max_result_", exp_1)
  exp_2 <- paste0("max_result_", exp_2)

  num_1_better <- 0
  num_2_better <- 0
  num_1_2_equal <- 0

  for (i in 1:length(MH_list_results)) {
    result_list <- get_experiment_result(i)
    my_diff <- result_list[[exp_1]] - result_list[[exp_2]]
    num_1_2_equal <- num_1_2_equal + sum(my_diff == 0)
    num_1_better <- num_1_better + sum(my_diff > 0)
    num_2_better <- num_2_better + sum(my_diff < 0)
  }

  my_out <- list()
  my_out[[paste0("better_", substring(exp_1, 12, nchar(exp_1)))]] <- num_1_better
  my_out[[paste0("better_", substring(exp_2, 12, nchar(exp_2)))]] <- num_2_better
  my_out[["equal"]] <- num_1_2_equal

  my_out
}

times_better("MH", "SA_0_01") # 183 times MH better, 45 times SA better
times_better("MH", "SA_0_1") # 134 times MH better, 90 times SA better
times_better("MH", "SA_10") # 161 times MH better, 63 times SA better
plot_results("SA")
plot_results("SA", focus = TRUE)

times_better("MH", "EA_Test1") # 50 times MH better, 188 times EA better
plot_results("EA")
plot_results("EA", focus = TRUE)

times_better("MH", "MH_sq_unequal") # 79 times MH better, 148 times sq better
times_better("MH", "MH_sq_equal") # 81 times MH better, 148 times sq better
times_better("MH", "MH_sq_loads") # 164 times MH better, 62 times sq better
plot_results("MH_sq")
plot_results("MH_sq", focus = TRUE)

times_better("MH_sq_unequal", "MH_sq_equal") # 118 times unequal better; 100 times equal better

#plot_results("MH_S")



for (i in 1:length(all_experiments)) {
  print(paste0("n = ", all_experiments[[i]]$n))
  print(paste0("p = ", attr(all_experiments[[i]]$true_perm, "size")))
  print(paste0("cicle length = ", length(all_experiments[[i]]$true_perm[[1]])))

  result_list <- get_experiment_result(i)

  print("How many times sq_unequal is bettern than MH:")
  print(exp(mean(result_list$max_result_MH_sq_unequal - result_list$max_result_MH)))
  print("How many times sq_unequal is bettern than sq_equal:")
  print(exp(mean(result_list$max_result_MH_sq_unequal - result_list$max_result_MH_sq_equal)))

  print("How many times EA is bettern than MH:")
  print(exp(mean(result_list$max_result_EA_Test1 - result_list$max_result_MH)))

  readline(prompt = "Press [enter] to continue")
}
