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

n <- 500

Z <- withr::with_seed(2022,
  code = MASS::mvrnorm(n, mu = mu, Sigma = sigma_matrix)
)
S <- (t(Z) %*% Z) / n

g <- gips(S, n, was_mean_estimated = FALSE)
g_MAP <- find_MAP(
  g, optimizer = "BF", save_all_perms = TRUE, return_probabilities = TRUE
)

g_MAP
options(scipen=999)
head(
  get_probabilities_from_gips(g_MAP),
  12
)
