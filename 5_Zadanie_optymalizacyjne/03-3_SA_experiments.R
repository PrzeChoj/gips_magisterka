optimizer <- "SA"
available_cores <- 90
F_call <- 10000

DATADIR <- file.path(".", "5_Zadanie_optymalizacyjne", "data")
source(file.path(DATADIR, "..", "03-0_setup_experiments.R"))

cooling_schedule <- function(i) {
  10
}

start_time <- Sys.time()
list_result_experiments <- apply_experiments(
  optimizer, available_cores, cooling_schedule = cooling_schedule
)
Sys.time() - start_time

SA_list_results <- widen_list_result_experiments(list_result_experiments)

save(SA_list_results,
  file = file.path(DATADIR, paste0("results_", optimizer, "_10_", F_call, ".rda"))
)
