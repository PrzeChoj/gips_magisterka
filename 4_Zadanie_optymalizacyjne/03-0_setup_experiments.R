#remotes::install_github("PrzeChoj/gips", ref = "altOptimizers")

library(gips)

DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")
load(file.path(DATADIR, "all_experiments.rda"))

source(file.path(DATADIR, "..", "00-utils.R"))

calculate_g_for_optimizer <- function(experiment_id, my_seed, optimizer, ...) {
  n <- all_experiments[[experiment_id]]$n
  true_perm <- all_experiments[[experiment_id]]$true_perm
  S_matrix_generator <- all_experiments[[experiment_id]]$S_matrix_generator

  S <- S_matrix_generator(my_seed, true_perm, n)

  g <- gips(S, n, was_mean_estimated = FALSE)
  g_optimized <- withr::with_seed(my_seed,
    code = find_MAP(
      g, optimizer = optimizer, max_iter = F_call,
      show_progress_bar = FALSE, ...
    )
  )

  g_optimized
}


perform_single_seed_experiment <- function(experiment_id_and_seed, optimizer, ...) {
  c_experiment_id_my_seed <- split_experiment_id_and_seed(experiment_id_and_seed)
  experiment_id <- c_experiment_id_my_seed[1]
  my_seed <- c_experiment_id_my_seed[2]

  calculate_g_for_optimizer(experiment_id, my_seed, optimizer, ...)
}

library(parallel)
numCores <- detectCores()
available_cores <- min(available_cores, numCores)

apply_experiments <- function(optimizer, available_cores, ...) {
  mclapply(
    1:(M * length(all_experiments)),
    function(experiment_id_and_seed) {
      perform_single_seed_experiment(experiment_id_and_seed, optimizer, ...)
    }, mc.cores = available_cores
  )
}

widen_list_result_experiments <- function(list_result_experiments) {
  my_widened_results <- vector("list", length(all_experiments))
  for (experiment_id in 1:length(all_experiments)) {
    my_widened_results[[experiment_id]] <- vector("list", M)
  }

  for (experiment_id_and_seed in 1:(M * length(all_experiments))) {
    c_experiment_id_my_seed <- split_experiment_id_and_seed(experiment_id_and_seed)
    experiment_id <- c_experiment_id_my_seed[1]
    my_seed <- c_experiment_id_my_seed[2]

    my_widened_results[[experiment_id]][[my_seed]] <- list_result_experiments[[experiment_id_and_seed]]
  }

  my_widened_results
}
