optimizer = "MH"

available_cores <- 47

F_call <- 10000

DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")

source(file.path(DATADIR, "..", "03-0_setup_experiments.R"))


start_time <- Sys.time()
list_result_experiments <- apply_experiments(optimizer, available_cores)
Sys.time() - start_time

MH_list_results <- widen_list_result_experiments(list_result_experiments)

save(MH_list_results,
  file = file.path(DATADIR, paste0("results_", optimizer, "_", F_call, ".rda"))
)
