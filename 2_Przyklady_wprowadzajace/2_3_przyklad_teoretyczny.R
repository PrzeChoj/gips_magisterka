library(gips)

# Prepare model, multivariate normal distribution
p <- 6
n <- 4
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


# Generate example data from a model:
Z <- withr::with_seed(2022,
  code = MASS::mvrnorm(n, mu = mu, Sigma = sigma_matrix)
)
# End of prepare model

plot(gips(sigma_matrix, 1), type = "heatmap") +
  ggplot2::labs(title = "To jest prawdziwa macierz kowariancji\nChcemy ją estymować w oparciu o jedynie n = 4 obserwacje", x = "", y = "")

dim(Z)
number_of_observations <- nrow(Z) # 4
p <- ncol(Z) # 6

# Calculate the covariance matrix from the data:
S <- (t(Z) %*% Z) / number_of_observations

g <- gips(S, number_of_observations, was_mean_estimated = FALSE)

plot(g, type = "heatmap") + ggplot2::ggtitle("Zwykły estymator macierzy kowariancji")

g_map <- find_MAP(g, optimizer = "brute_force")

g_map

summary(g_map)$n0
summary(g_map)$n0 <= number_of_observations # 1 <= 4

S_projected <- project_matrix(S, g_map)
S_projected

# Plot the found matrix:
plot(g_map, type = "heatmap") + ggplot2::ggtitle("Macierz kowariancji wyestymowana przy pomocy `gips`")
