optimizer = "MH"

#F_call <- 10000
F_call <- 3

DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")

source(file.path(DATADIR, "..", "03-0_setup_experiments.R"))


library(parallel)
numCores <- detectCores()
available_cores <- min(available_cores, numCores)

start_time <- Sys.time()
list_result_experiments <- mclapply(
  1:(M * length(all_experiments)),
  function(experiment_id_and_seed) {
    perform_single_seed_experiment(experiment_id_and_seed, optimizer)
  }
)
Sys.time() - start_time

MH_list_results <- widen_list_result_experiments(list_result_experiments)

save(MH_list_results,
  file = file.path(DATADIR, paste0("results_MH_", F_call, ".rda"))
)

MH_list_results[[4]][[10]] # (8,23)(12,16); 14814.077
MH_list_results[[14]][[3]] # (4,7)(8,26); 1.295e+50
MH_list_results[[27]][[1]] # (43,87); 3.905e+16
