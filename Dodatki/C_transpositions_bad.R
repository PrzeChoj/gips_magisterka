library(gips)
options(scipen = 999)

n <- 500
p <- 6
mu <- numeric(p)

my_seed <- 2022

get_S_from_sigma <- function(sigma_matrix) {
  Z <- withr::with_seed(my_seed,
    code = MASS::mvrnorm(n, mu = mu, Sigma = sigma_matrix)
  )

  (t(Z) %*% Z) / n
}

get_g_BF <- function(S) {
  g <- gips(S, n, was_mean_estimated = FALSE)
  g_BF <- find_MAP(
    g, optimizer = "BF", save_all_perms = TRUE, return_probabilities = TRUE
  )

  g_BF
}

get_g_MH <- function(S) {
  g <- gips(S, n, was_mean_estimated = FALSE)
  g_MH <- find_MAP(
    g, max_iter = 10000, optimizer = "MH",
    save_all_perms = TRUE, return_probabilities = TRUE
  )

  g_MH
}


#####
# Example that will never* leave ()
sigma_matrix <- matrix(
  data = c(
    1.1, 0.8, 0.6, 0.6, 0.8, 0.1,
    0.8, 1.1, 0.8, 0.6, 0.6, 0.1,
    0.6, 0.8, 1.1, 0.8, 0.6, 0.1,
    0.6, 0.6, 0.8, 1.1, 0.8, 0.1,
    0.8, 0.6, 0.6, 0.8, 1.1, 0.1,
    0.1, 0.1, 0.1, 0.1, 0.1, 2.1
  ),
  nrow = p, byrow = TRUE
) # sigma_matrix is a matrix invariant under permutation (1,2,3,4,5)(6)

S <- get_S_from_sigma(sigma_matrix)
g_BF <- get_g_BF(S)

g_BF
head(get_probabilities_from_gips(g_BF), 8)

g_MH <- get_g_MH(S)
get_probabilities_from_gips(g_MH)

my_list <- get_probabilities_from_gips(g_BF)
my_list[["()"]] / my_list[["(1,3)"]] # 646 816 878 672 322 112 782 336 > 10^22


#####
# Example that will stop at (1,3)(2,4)
sigma_matrix <- matrix(
  data = c(
    1.1, 0.8, 0.6, 0.8, 0.3, 0.1,
    0.8, 1.1, 0.8, 0.6, 0.3, 0.1,
    0.6, 0.8, 1.1, 0.8, 0.3, 0.1,
    0.8, 0.6, 0.8, 1.1, 0.3, 0.1,
    0.3, 0.3, 0.3, 0.3, 0.7, 0.5,
    0.1, 0.1, 0.1, 0.1, 0.5, 2.1
  ),
  nrow = p, byrow = TRUE
) # sigma_matrix is a matrix invariant under permutation (1,2,3,4)(5)(6)

S <- get_S_from_sigma(sigma_matrix)
g_BF <- get_g_BF(S)

g_BF
head(get_probabilities_from_gips(g_BF), 8)

g_MH <- get_g_MH(S)
get_probabilities_from_gips(g_MH)


#####
# Example, that will work, because the (1,2) is subspace of (1,2,3):
sigma_matrix <- matrix(
  data = c(
    1.1, 0.8, 0.8, 0.2, 0.3, 0.1,
    0.8, 1.1, 0.8, 0.2, 0.3, 0.1,
    0.8, 0.8, 1.1, 0.2, 0.3, 0.1,
    0.2, 0.2, 0.2, 1.7, 0.1, 0.8,
    0.3, 0.3, 0.3, 0.1, 0.7, 0.5,
    0.1, 0.1, 0.1, 0.8, 0.5, 2.1
  ),
  nrow = p, byrow = TRUE
) # sigma_matrix is a matrix invariant under permutation (1,2,3)(4)(5)(6)

S <- get_S_from_sigma(sigma_matrix)
g_BF <- get_g_BF(S)

g_BF
head(get_probabilities_from_gips(g_BF), 8)

g_MH <- get_g_MH(S)
get_probabilities_from_gips(g_MH)
