library(gips)
library(ggplot2)

my_seed <- 2024
set.seed(my_seed)

n <- 500 # 30 for the comment in summary
p <- 6
mu <- numeric(p)


get_S_from_sigma <- function(sigma_matrix) {
  Z <- withr::with_seed(my_seed,
    code = MASS::mvrnorm(n, mu = mu, Sigma = sigma_matrix)
  )

  (t(Z) %*% Z) / n
}

get_g_BF <- function(S) {
  g <- gips(S, n, was_mean_estimated = FALSE)
  g_BF <- find_MAP(
    g, optimizer = "BF",
    save_all_perms = TRUE, return_probabilities = TRUE
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

get_g_MH_sqrt <- function(S) {
  g <- gips(S, n, was_mean_estimated = FALSE)
  g_MH_sqrt <- find_MAP(
    g, max_iter = 10000, optimizer = "Metropolis_Hastings_with_sqrt",
    save_all_perms = TRUE,
    alpha = 0.2, beta = 0.5
  )

  g_MH_sqrt
}

plot_matrix <- function(sigma_matrix, title) {
  gips:::pretty_plot_matrix(sigma_matrix) +
    labs(
      title = title,
      fill = "kowariancja"
    )
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

plot_matrix_5 <- plot_matrix(
  sigma_matrix,
  bquote(atop("Prawdziwa macierz kowariancji                              ", paste("niezmiennicza względem permutacji ", bold("(1,2,3,4,5)(6)"))))
)
ggplot2::ggsave(
  file.path(".", "plots", "Dod_C_5.png"),
  plot_matrix_5,
  width = 13, height = 12,
  units = "cm"
)

S <- get_S_from_sigma(sigma_matrix)
g_BF <- get_g_BF(S)

g_BF
head(get_probabilities_from_gips(g_BF), 8)

g_MH <- get_g_MH(S)
g_MH
get_probabilities_from_gips(g_MH)


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

plot_matrix_4 <- plot_matrix(
  sigma_matrix,
  bquote(atop("Prawdziwa macierz kowariancji                               ", paste("niezmiennicza względem permutacji ", bold("(1,2,3,4)(5)(6)"))))
)
ggplot2::ggsave(
  file.path(".", "plots", "Dod_C_4.png"),
  plot_matrix_4,
  width = 13, height = 12,
  units = "cm"
)

S <- get_S_from_sigma(sigma_matrix)
g_BF <- get_g_BF(S)

g_BF
head(get_probabilities_from_gips(g_BF), 8)

g_MH <- get_g_MH(S)
g_MH
get_probabilities_from_gips(g_MH)

g_MH_sqrt <- get_g_MH_sqrt(S)
g_MH_sqrt


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

plot_matrix_3 <- plot_matrix(
  sigma_matrix,
  bquote(atop("Prawdziwa macierz kowariancji                                ", paste("niezmiennicza względem permutacji ", bold("(1,2,3)(4)(5)(6)"))))
)
ggplot2::ggsave(
  file.path(".", "plots", "Dod_C_3.png"),
  plot_matrix_3,
  width = 13, height = 12,
  units = "cm"
)

S <- get_S_from_sigma(sigma_matrix)
g_BF <- get_g_BF(S)

g_BF
head(get_probabilities_from_gips(g_BF), 6)

g_MH <- get_g_MH(S)
g_MH
get_probabilities_from_gips(g_MH)
