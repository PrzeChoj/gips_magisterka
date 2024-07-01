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

my_plot <- function(my_lambdas, p_value_text_place) {
  plot(ecdf(my_lambdas), xlim = c(0, 50), main = paste0("Wykres ECDF dla n = ", n))
  lines(0:M / (M/50), y = pchisq(0:M / (M/50), p*(p+1)/2 - 4), col = "red")
  legend(25, 0.6, legend=c("chi^2_17", "lambda_LR"), col=c("red", "black"), lty=c(1,1))
  my_p_val <- ks.test(my_lambdas, "pchisq", p*(p+1)/2 - 4)
  text(p_value_text_place, 0.1, paste("p-value =", format(my_p_val$p.value, digits = 2, scientific = FALSE)),
       cex=1, pos=3,col="blue")
}

library(parallel)
numCores <- detectCores()
available_cores <- 7

M <- 1000

n <- 10
my_lambdas <- unlist(mclapply(1:M, function(my_seed){get_lambda(my_seed, n, FALSE)}, mc.cores = available_cores))
my_plot(my_lambdas, 29)
# save as plots/test_n_10.png

n <- 30
my_lambdas <- unlist(mclapply(1:M, function(my_seed){get_lambda(my_seed, n, FALSE)}, mc.cores = available_cores))
my_plot(my_lambdas, 36)
# save as plots/test_n_30.png

n <- 150
my_lambdas <- unlist(mclapply(1:M, function(my_seed){get_lambda(my_seed, n, FALSE)}, mc.cores = available_cores))
my_plot(my_lambdas, 30)
# save as plots/test_n_150.png
