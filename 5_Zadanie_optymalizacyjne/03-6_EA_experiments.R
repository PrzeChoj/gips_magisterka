optimizer <- "EA"
available_cores <- 90
F_call <- 10000

DATADIR <- file.path(".", "5_Zadanie_optymalizacyjne", "data")
source(file.path(DATADIR, "..", "03-0_setup_experiments.R"))

#####
pop_size = 100
init = "random_close"
p_0 = 0.5
success_treshold = 0.031
a = 0.3
k_max = 7
tournament_part = 0.35

start_time <- Sys.time()
list_result_experiments <- apply_experiments(
  optimizer, available_cores = available_cores, pop_size = pop_size, success_treshold = success_treshold,
  p_0 = p_0, a = a, k_max = k_max, tournament_part = tournament_part,
  max_f_calls = F_call, init = init
)
Sys.time() - start_time

EA_list_results <- widen_list_result_experiments(list_result_experiments)

save(EA_list_results,
  file = file.path(DATADIR, paste0("results_", optimizer, "_Test1_F", F_call, ".rda"))
)
