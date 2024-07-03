M <- 10 # number of seeds to compute the value for

# for 01:
# Make true matrices

#' true_perm <- gips::gips_perm("(12345)", 10)
#' A <- get_sigma_wishart_perm(true_perm, 2022)
#' gips:::pretty_plot_matrix(A)
get_sigma_wishart_perm <- function(perm, my_seed) {
  p <- attr(perm, "size")
  Z <- withr::with_seed(my_seed,
     code = MASS::mvrnorm(n = p, mu = numeric(p), Sigma = diag(1, p))
  )
  sigma_wishart <- (t(Z) %*% Z) / p
  project_matrix(sigma_wishart, perm)
}

#' true_perm <- gips::gips_perm("(12345)", 10)
#' S_matrix_generator(1234, true_perm, 15)
S_matrix_generator <- function(my_seed, true_perm, n) {
  true_matrix <- get_sigma_wishart_perm(true_perm, my_seed)
  p <- attr(true_perm, "size")
  Z <- withr::with_seed(my_seed,
    code = MASS::mvrnorm(n = n, mu = numeric(p), Sigma = true_matrix)
  )
  (t(Z) %*% Z) / n
}
