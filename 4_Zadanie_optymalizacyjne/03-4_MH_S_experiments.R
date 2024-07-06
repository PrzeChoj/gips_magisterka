optimizer = "MH_S"
available_cores <- 47
F_call <- 10000
theta <- seq(0, 0.99, length.out = F_call)

DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")
source(file.path(DATADIR, "..", "03-0_setup_experiments.R"))

start_time <- Sys.time()
list_result_experiments <- apply_experiments(optimizer, available_cores, theta = theta)
Sys.time() - start_time

MH_S_list_results <- widen_list_result_experiments(list_result_experiments)

save(MH_S_list_results,
  file = file.path(DATADIR, paste0("results_", optimizer, "_thetaseq_", F_call, ".rda"))
)
