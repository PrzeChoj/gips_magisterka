DATADIR <- file.path(".", "5_Zadanie_optymalizacyjne", "data")

library(gips)

set.seed(2022)

source(file.path(DATADIR, "..", "00-utils.R"))


# Set p and permutations
p_small <- 10 # has to be even
p_small_structure_large_perm <- gips_perm(permutations::rcyc(1, p_small), p_small)
p_small_structure_moderate_perm <- gips_perm(permutations::rcyc(1, p_small / 2), p_small)
p_small_structure_no_perm <- gips_perm("", p_small)

p_moderate <- 30 # has to be even
p_moderate_structure_large_perm <- gips_perm(permutations::rcyc(1, p_moderate), p_moderate)
p_moderate_structure_moderate_perm <- gips_perm(permutations::rcyc(1, p_moderate / 2), p_moderate)
p_moderate_structure_no_perm <- gips_perm("", p_moderate)

p_large <- 100 # has to be even
p_large_structure_large_perm <- gips_perm(permutations::rcyc(1, p_large), p_large)
p_large_structure_moderate_perm <- gips_perm(permutations::rcyc(1, p_large / 2), p_large)
p_large_structure_no_perm <- gips_perm("", p_large)

all_perms <- list(
  p_small_structure_large_perm, p_small_structure_moderate_perm, p_small_structure_no_perm,
  p_moderate_structure_large_perm, p_moderate_structure_moderate_perm, p_moderate_structure_no_perm,
  p_large_structure_large_perm, p_large_structure_moderate_perm, p_large_structure_no_perm
)


# Set n
n_small <- 15
n_moderate <- 50
n_big <- 200

all_n <- list(
  n_small, n_moderate, n_big
)


# Experiments:
all_experiments <- vector("list", length(all_n) * length(all_perms))
for (i in 1:length(all_n)) {
  for (j in 1:length(all_perms)) {
    experiment_index <- (i - 1) * length(all_perms) + j

    all_experiments[[experiment_index]] <- list(
      n = all_n[[i]],
      true_perm = all_perms[[j]]
    )
  }
}


# Save:
save(all_experiments, get_sigma_wishart_perm,
  file = file.path(DATADIR, "all_experiments.rda")
)
