# for 01:
# Make true matrices
stabilizer <- 0.1

get_sigma_wishart_perm <- function(perm, my_seed) {
  p <- attr(perm, "size")
  Z <- withr::with_seed(my_seed,
                        code = MASS::mvrnorm(n = p, mu = numeric(p), Sigma = diag(1, p))
  )
  sigma_wishart <- (t(Z) %*% Z) / p
  project_matrix(sigma_wishart, perm)
}

change_bigest_eigen_value <- function(A, stabilizer) {
  r <- eigen(A, TRUE)
  r$values[1] <- stabilizer
  r$vectors %*% diag(r$values) %*% t(r$vectors)
}

true_matrix_generator <- function(my_seed, true_perm, stabilizer) {
  change_bigest_eigen_value(get_sigma_wishart_perm(true_perm, my_seed), stabilizer)
}

S_matrix_generator <- function(my_seed, true_perm, stabilizer, n) {
  true_matrix <- true_matrix_generator(my_seed, true_perm, stabilizer)
  p <- attr(true_perm, "size")
  Z <- withr::with_seed(my_seed,
    code = MASS::mvrnorm(n = n, mu = numeric(p), Sigma = true_matrix)
  )
  (t(Z) %*% Z) / n
}
