library(gips)

# Prepare model, multivariate normal distribution
p <- 6
mu <- numeric(p)
sigma_matrix <- matrix(
  data = c(
    1.1, 0.8, 0.6, 0.4, 0.6, 0.8,
    0.8, 1.1, 0.8, 0.6, 0.4, 0.6,
    0.6, 0.8, 1.1, 0.8, 0.6, 0.4,
    0.4, 0.6, 0.8, 1.1, 0.8, 0.6,
    0.6, 0.4, 0.6, 0.8, 1.1, 0.8,
    0.8, 0.6, 0.4, 0.6, 0.8, 1.1
  ),
  nrow = p, byrow = TRUE
) # sigma_matrix is a matrix invariant under permutation (1,2,3,4,5,6)

real_perm <- "(123456)"

get_lambda <- function(my_seed, n, show_progress_bar = FALSE) {
  # Generate example data from a model:
  Z <- withr::with_seed(my_seed,
    code = MASS::mvrnorm(n, mu = mu, Sigma = sigma_matrix)
  )
  S <- (t(Z) %*% Z) / n

  S_projected <- project_matrix(S, real_perm)

  lambda_LR <- n*(determinant(S_projected)$modulus - determinant(S)$modulus)

  lambda_LR
}

my_plot <- function(my_lambdas, p_value_text_place, legend_place) {
  my_ecdf <- ecdf(my_lambdas)
  plot(my_ecdf, xlim = c(0, 50), main = paste0("Wykres ECDF dla n = ", n), axes = FALSE)
  axis(2, at = c(seq(0, 1, by = 0.2), 0.95), las=2)
  box()
  abline(h = 0.95, col = "gray")
  lines(0:M / (M/50), y = pchisq(0:M / (M/50), p*(p+1)/2 - 4), col = "red")
  chisq_p_value <- qchisq(0.95, p*(p+1)/2 - 4)
  abline(v = chisq_p_value)
  i_p_value_ecdf <- which.max(my_ecdf(seq(0, 50, by = 0.1)) > 0.95) # first index of TRUE
  ecdf_p_value <- seq(0, 50, by = 0.1)[i_p_value_ecdf]
  abline(v = ecdf_p_value)
  axis(1, at = c(seq(0, 50, by = 10), round(chisq_p_value, 1), ecdf_p_value), las=2)
  legend(legend_place, 0.6, legend=c("chi^2_17", "lambda_LR"), col=c("red", "black"), lty=c(1, 1))
  my_p_val <- ks.test(my_lambdas, "pchisq", p*(p+1)/2 - 4)
  text(p_value_text_place, 0.1, paste("p-value =", format(my_p_val$p.value, digits = 2, scientific = FALSE)),
       cex = 1, pos = 3, col = "blue")
}

library(parallel)
numCores <- detectCores()
available_cores <- 7

M <- 1000

n <- 10
my_lambdas <- unlist(mclapply(1:M, function(my_seed){get_lambda(my_seed, n, FALSE)}, mc.cores = available_cores))
my_plot(my_lambdas, 31.5, 28)
# save as plots/test_n_10.png

n <- 30
my_lambdas <- unlist(mclapply(1:M, function(my_seed){get_lambda(my_seed, n, FALSE)}, mc.cores = available_cores))
my_plot(my_lambdas, 42, 33)
# save as plots/test_n_30.png

n <- 150
my_lambdas <- unlist(mclapply(1:M, function(my_seed){get_lambda(my_seed, n, FALSE)}, mc.cores = available_cores))
my_plot(my_lambdas, 34.5, 30)
# save as plots/test_n_150.png
