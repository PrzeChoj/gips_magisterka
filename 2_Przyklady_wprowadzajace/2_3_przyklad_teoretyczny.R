library(gips)

my_rank <- function (my_S) {
  sum(eigen(my_S, TRUE, TRUE)$values > 0.0001)
}

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

prawdziwa_CoV_ggplot <- plot(gips(sigma_matrix, 1), type = "heatmap") +
  ggplot2::labs(
    title = "To jest prawdziwa macierz kowariancji",
    subtitle = "Chcemy ją estymować w oparciu o jedynie n = 4 obserwacje", x = "", y = "",
    fill = "kowariancja") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 18),
    plot.subtitle = ggplot2::element_text(face = "bold", size = 14),
    legend.text = ggplot2::element_text(face = "bold", size = 10),
    axis.text.y = ggplot2::element_text(face = "bold", size = 17),
    axis.text.x = ggplot2::element_text(face = "bold", size = 17)
  )
prawdziwa_CoV_ggplot
# Figure 2.5:
ggplot2::ggsave(
  file.path(".", "plots", "exp_theory_real.png"),
  prawdziwa_CoV_ggplot,
  width = 21.3, height = 18,
  units = "cm"
)

dim(Z)
number_of_observations <- nrow(Z) # 4
p <- ncol(Z) # 6

# Calculate the covariance matrix from the data:
S <- (t(Z) %*% Z) / number_of_observations
my_rank(S) # matrix S is of rank 4

# sqrt Mean Squared Error
norm(S - sigma_matrix, "F")^2 / (p*p) # 0.31

g <- gips(S, number_of_observations, was_mean_estimated = FALSE)

S_CoV_ggplot <- plot(g, type = "heatmap") +
  ggplot2::labs(
    title = "Zwykły estymator macierzy kowariancji",
    fill = "kowariancja") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 18),
    plot.subtitle = ggplot2::element_text(face = "bold", size = 14),
    legend.text = ggplot2::element_text(face = "bold", size = 10),
    axis.text.y = ggplot2::element_text(face = "bold", size = 17),
    axis.text.x = ggplot2::element_text(face = "bold", size = 17)
  )
S_CoV_ggplot
# Figure 2.6:
ggplot2::ggsave(
  file.path(".", "plots", "exp_theory_S.png"),
  S_CoV_ggplot,
  width = 21.3, height = 18,
  units = "cm"
)

g_map <- find_MAP(g, optimizer = "brute_force")
g_map

summary(g_map)$n0
summary(g_map)$n0 <= number_of_observations # 1 <= 4

S_projected <- project_matrix(S, g_map)
S_projected

my_rank(S_projected) # matrix S_projected is of rank 6

# Mean Squared Error
norm(S_projected - sigma_matrix, "F")^2 / (p*p) # 0.047


# Plot the found matrix:
S_gips_CoV_ggplot <- plot(g_map, type = "heatmap") +
  ggplot2::labs(
    title = "Macierz kowariancji wyestymowana przy pomocy `gips`",
    fill = "kowariancja") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 18),
    plot.subtitle = ggplot2::element_text(face = "bold", size = 14),
    legend.text = ggplot2::element_text(face = "bold", size = 10),
    axis.text.y = ggplot2::element_text(face = "bold", size = 17),
    axis.text.x = ggplot2::element_text(face = "bold", size = 17)
  )
S_gips_CoV_ggplot
# Figure 2.7:
ggplot2::ggsave(
  file.path(".", "plots", "exp_theory_S_gips.png"),
  S_gips_CoV_ggplot,
  width = 21.3, height = 18,
  units = "cm"
)
