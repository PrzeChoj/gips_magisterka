# For M = 10, approx 75 seconds on 47 cores of sr-3:

available_cores <- 47
M <- 10 # number of seeds to compute the value for

library(gips)
library(magrittr)

DATADIR <- file.path(".", "4_Zadanie_optymalizacyjne", "data")
load(file.path(DATADIR, "all_experiments.rda"))

source(file.path(DATADIR, "..", "00-utils.R"))


K <- 30 # number of random permutations to draw; 100 and 30 are very similar, so 30 is enought
get_random_perm_value <- function(i, my_seed) {
  n <- all_experiments[[i]]$n
  true_perm <- all_experiments[[i]]$true_perm

  p <- attr(true_perm, "size")
  S <- S_matrix_generator(my_seed, true_perm, n)

  random_perms <- withr::with_seed(my_seed,
    code = permutations::rperm(K, r = p)
  )

  my_mean <- 0
  for (k in 1:K) {
    g <- gips(S, n, was_mean_estimated = FALSE, perm = gips_perm(random_perms[k], p))
    my_mean <- my_mean + log_posteriori_of_gips(g) / K
  }

  my_mean
}

mean_random_perm_log_posteriori <- numeric(length(all_experiments))

library(parallel)
numCores <- detectCores()
available_cores <- min(available_cores, numCores)

start_time <- Sys.time()
list_random_perm_log_posteriori <- vector("list", M)
for (i in 1:length(all_experiments)) {
  print(i)
  n <- all_experiments[[i]]$n
  true_perm <- all_experiments[[i]]$true_perm
  true_matrix_generator <- all_experiments[[i]]$true_matrix_generator

  list_random_perm_log_posteriori[[i]] <- as.numeric(mclapply(1:M, function(my_seed) {
    get_random_perm_value(i, my_seed)
  }, mc.cores = available_cores))
}
Sys.time() - start_time

df_random_perm_log_posteriori <- as.data.frame(list_random_perm_log_posteriori)
colnames(df_random_perm_log_posteriori) <- as.character(1:length(all_experiments))


# Save:
save(df_random_perm_log_posteriori,
  file = file.path(DATADIR, "df_random_perm_log_posteriori.rda")
)
