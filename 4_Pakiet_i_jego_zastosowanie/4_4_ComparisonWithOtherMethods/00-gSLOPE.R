# Code for gSLOPE, courtesy of Malgorzata Bogdan

lambdaBH <- function(p, n, alpha = 0.2) {
  pBH <- p * (p - 1) / 2
  k <- 1:pBH
  fractionSeq <- qt(1 - alpha * k / 2 / pBH, df = n - 2) / sqrt(n - 2 + qt(1 - alpha * k / 2 / pBH, df = n - 2)^2)

  c(rep(fractionSeq[1], p), rep(fractionSeq, each = 2))
}

proximity_matrix <- function(matrix_in, lambdas) {
  output <- matrix_in
  precision_entries <- matrix_in[lower.tri(matrix_in, FALSE)]
  calculated_entries <- grpSLOPE::prox_sorted_L1(as.matrix(precision_entries),
    lambdas,
    method = c("c")
  )
  output[lower.tri(output, FALSE)] <- calculated_entries
  output <- t(output)
  output[lower.tri(output, FALSE)] <- calculated_entries

  output
}

gslope_new <- function(
    sample_cov,
    lambdas,
    rho = 1.1,
    max_iter = 500,
    epsilon = 1e-04) {
  if (!(nrow(sample_cov) == ncol(sample_cov))) {
    stop("Covariance matrix must be square.")
  }

  # Parameters initialization
  Z <- sample_cov # Initialized to zero, probably it is the best choice
  Y <- Z
  X <- diag(nrow(sample_cov))

  # Start iteration
  for (iter in 1:max_iter) {
    C_tilde <- Y - Z - (sample_cov / rho)

    # Perform the eigenvalue decomposition
    C_eigen <- eigen(C_tilde, symmetric = TRUE)
    C_eigen_val <- C_eigen$val # Eigenvalues
    C_eigen_vec <- C_eigen$vec # Eigenvectors

    # Formula implementation
    F_rho <- 0.5 * diag(C_eigen_val + sqrt(C_eigen_val^2 + 4 / rho))
    X <- C_eigen_vec %*% F_rho %*% t(C_eigen_vec)

    Y_old <- Y
    Y <- proximity_matrix(X + Z, lambdas / rho)

    # Update step
    Z <- Z + rho * (X - Y)

    # Compute the primal and dual gap
    primal_residual <- norm(X - Y, type = "F")
    dual_residual <- norm(rho * (Y - Y_old), type = "F")

    # Stop condition
    if (primal_residual < epsilon & dual_residual < epsilon) {
      break
    }
  }

  X[abs(X) < 1e-04] <- 0 # Thresholding if abs(entries) <= 10e-4

  list(
    precision_matrix = X,
    iterations = iter,
    prim_res = primal_residual,
    dual_res = dual_residual
  )
}
