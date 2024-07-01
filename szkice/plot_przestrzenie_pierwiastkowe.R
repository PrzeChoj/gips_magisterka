set.seed(1)
perm_size <- 10
mu <- numeric(perm_size)

get_sigma_matrix <- function(my_perm) {
  A <- matrix(rnorm(perm_size * perm_size), nrow = perm_size)
  B <- project_matrix(t(A) %*% A, my_perm)
  B_smallest_eig <- min(eigen(B, TRUE, TRUE)$values)

  B - diag(B_smallest_eig - 0.1, perm_size, perm_size)
}

random_perm_1 <- permutations::as.cycle("(1,2,3,4,5,6,7,8,9,10)")
sigma_matrix_1 <- get_sigma_matrix(random_perm_1)

random_perm_2 <- permutations::as.cycle("(1,3,5,7,9)(2,4,6,8,10)")
sigma_matrix_2 <- get_sigma_matrix(random_perm_2)

p1 <- gips:::pretty_plot_matrix(sigma_matrix_1)
p2 <- gips:::pretty_plot_matrix(sigma_matrix_2)

gridExtra::grid.arrange(p1, p2, ncol=2)

random_perm_3 <- permutations::as.cycle("(1,2,3,4,5)(6,7,8,9,10)")
sigma_matrix_3 <- {
  A <- matrix(rnorm(perm_size * perm_size), nrow = perm_size)
  B <- project_matrix(t(A) %*% A, random_perm_3)
  B_smallest_eig <- min(eigen(B, TRUE, TRUE)$values)

  B - diag(B_smallest_eig - 0.1, perm_size, perm_size)
}

p3 <- gips:::pretty_plot_matrix(sigma_matrix_3)
gridExtra::grid.arrange(p1, p3, ncol=2)
