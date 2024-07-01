available_cores <- 7
F_call <- 10000 # number of function call
#F_call <- 1000
#F_call <- 3

library(gips)

DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")
load(file.path(DATADIR, "all_experiments.rda"))

M <- 25 # defined in 02.R

calculate_g_MH <- function(experiment_id, my_seed) {
  n <- all_experiments[[experiment_id]]$n
  true_perm <- all_experiments[[experiment_id]]$true_perm
  S_matrix_generator <- all_experiments[[experiment_id]]$S_matrix_generator

  S <- S_matrix_generator(my_seed, true_perm, n)

  g <- gips(S, n, was_mean_estimated = FALSE)
  g_MH <- withr::with_seed(my_seed,
    code = find_MAP(g, optimizer = "MH", max_iter = F_call, show_progress_bar = FALSE)
  )

  g_MH
}

#' @examples
#' my_seed <- 3
#' experiment_id <- 1
#' # then:
#' experiment_id_and_seed <- 3
#'
#' my_seed <- 3
#' experiment_id <- 2
#' # then:
#' experiment_id_and_seed <- M+3
split_experiment_id_and_seed <- function(experiment_id_and_seed) {
  my_seed <- experiment_id_and_seed %% M
  if (my_seed == 0){
    my_seed <- M
  }
  experiment_id <- (experiment_id_and_seed - my_seed) / M + 1

  c(experiment_id, my_seed)
}

perform_single_seed_experiment <- function(experiment_id_and_seed) {
  c_experiment_id_my_seed <- split_experiment_id_and_seed(experiment_id_and_seed)
  experiment_id <- c_experiment_id_my_seed[1]
  my_seed <- c_experiment_id_my_seed[2]

  calculate_g_MH(experiment_id, my_seed)
}

library(parallel)
numCores <- detectCores()
available_cores <- min(available_cores, numCores)
start_time <- Sys.time()
list_result_experiments <- mclapply(
  1:(M * length(all_experiments)),
  perform_single_seed_experiment
)
Sys.time() - start_time


my_MH_list_results <- vector("list", length(all_experiments))
for (experiment_id in 1:length(all_experiments)) {
  my_MH_list_results[[experiment_id]] <- vector("list", M)
}

for (experiment_id_and_seed in 1:(M * length(all_experiments))) {
  c_experiment_id_my_seed <- split_experiment_id_and_seed(experiment_id_and_seed)
  experiment_id <- c_experiment_id_my_seed[1]
  my_seed <- c_experiment_id_my_seed[2]

  my_MH_list_results[[experiment_id]][[my_seed]] <- list_result_experiments[[experiment_id_and_seed]]
}

save(my_MH_list_results,
  file = file.path(DATADIR, "my_MH_list_results.rda")
)
