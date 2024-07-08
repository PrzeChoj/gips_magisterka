optimizer <- "MH_sq"
available_cores <- 47
F_call <- 100

alpha <- 0.1
beta <- 0.1

DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")
source(file.path(DATADIR, "..", "03-0_setup_experiments.R"))

start_time <- Sys.time()
list_result_experiments <- apply_experiments(optimizer, available_cores, alpha = alpha, beta = beta)
Sys.time() - start_time

MH_S_list_results <- widen_list_result_experiments(list_result_experiments)

save(MH_S_list_results,
  file = file.path(DATADIR, paste0("results_", optimizer, "_a0_", char_after_decimal(alpha), "_b0_", char_after_decimal(beta), "_F", F_call, ".rda"))
)
