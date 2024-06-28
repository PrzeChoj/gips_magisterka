set.seed(1)
perm_size <- 25
mu <- runif(perm_size, -10, 10) # Assume we don't know the mean
random_perm <- permutations::rcyc(1, 25)
sigma_matrix <- {
  A <- matrix(rnorm(perm_size * perm_size), nrow = perm_size)
  B <- project_matrix(t(A) %*% A, random_perm)
  B_smallest_eig <- min(eigen(B, TRUE, TRUE)$values)

  B - diag(B_smallest_eig - 0.1, perm_size, perm_size)
}

# sigma_matrix is the real covariance matrix, that we want to estimate
number_of_observations <- 20
Z <- withr::with_seed(2022,
                      code = MASS::mvrnorm(number_of_observations, mu = mu, Sigma = sigma_matrix)
)

S <- cov(Z) # Assume we have to estimate the mean

g <- gips(S, number_of_observations)
g_old <- g

while (TRUE) {
  g_new <- find_MAP(g_old, max_iter = 1000, show_progress_bar = FALSE, optimizer = "Metropolis_Hastings")

  par(mfrow=c(1, 2))
  plot(g_new, logarithmic_x = TRUE, type = "both", show_legend = FALSE)
  plot(g_new, logarithmic_x = TRUE, type = "n0", logarithmic_y = FALSE, show_legend = FALSE)

  compare_log <- compare_log_posteriories_of_perms(g_new, g_old, print_output = FALSE)
  if (compare_log != 0) {
    cat(c("|", sprintf("%.2f", compare_log), "|"))
  } else {
    cat(".")
  }

  g_old <- g_new
}

g_best <- gips(S, number_of_observations, perm = random_perm)
compare_posteriories_of_perms(g_best, g_new)
