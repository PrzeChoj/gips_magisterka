optimizer = "MH_S"
available_cores <- 47
F_call <- 10000
#theta <- 0.9; theta_type <- paste0("Const_0_", substr(as.character(theta), 3, nchar(as.character(theta))))
#theta <- seq(0, 0.99, length.out = F_call); theta_type <- "lin"
#theta <- -cos(seq(0, pi, length.out = F_call))/2 + 0.5; theta_type <- "cos"
#theta <- -cos(log(1:F_call) * pi / log(F_call))/2 + 0.5; theta_type <- "logcos"

DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")
source(file.path(DATADIR, "..", "03-0_setup_experiments.R"))

start_time <- Sys.time()
list_result_experiments <- apply_experiments(optimizer, available_cores, theta = theta)
Sys.time() - start_time

MH_S_list_results <- widen_list_result_experiments(list_result_experiments)

save(MH_S_list_results,
  file = file.path(DATADIR, paste0("results_", optimizer, "_", theta_type, "_", F_call, ".rda"))
)
