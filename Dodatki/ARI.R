# TABLE 5, Graczyk 2022

library(magrittr)
library(gips)
library(mclust)

library(parallel)
numCores <- detectCores()
# available_cores <- 7
# available_cores <- 34

# Prepare model, multivariate normal distribution
p <- 10
n <- 20
mu <- numeric(p)
sigma_matrix <- diag(rep(1 + 1 / p, p))
for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    sigma_matrix[i, j] <- 1 - min((j - i), p - (j - i)) / p
    sigma_matrix[j, i] <- sigma_matrix[i, j]
  }
} # sigma_matrix is invariant under (1, 2, ..., p-1, p).
# gips:::pretty_plot_matrix(sigma_matrix)

get_permutation <- function() {
  Z <- MASS::mvrnorm(n, mu = mu, Sigma = sigma_matrix)

  g <- gips(t(Z) %*% Z, number_of_observations = n, D_matrix = diag(p))
  g_MH <- find_MAP(g, max_iter = max_iter_MH, optimizer = "MH")

  as.character(g_MH)
}

max_iter_MH <- 100000
num_rep <- 100
start_time <- Sys.time()
my_perms <- 1:num_rep %>% mclapply(function(my_seed) {
  withr::with_seed(my_seed,
    code = get_permutation()
  )
}, mc.cores = available_cores)
Sys.time() - start_time # 25 minutes on 7 cores; 8 minutes on 34 cores
my_perms <- as.character(my_perms)

my_perms <- c(
  "(1,2,10,9,8,7,6,5,4,3)", "(1,2,3,6,8,9,10,4,5,7)", "(1,7,2,6,8,3,9,4,10,5)",
  "(1,10,9,8,7,6,5,4,3,2)", "(1,9,5,8,2,6,4,10,3,7)", "(1,4,8,3,7,2,9,5,10,6)",
  "(1,6,2,8,3,9,5,10,4,7)", "(1,3,9,2,10,6,8,4,7,5)", "(1,5,9,3,8,2,6,10,4,7)",
  "(1,10,9,8,5,7,6,4,3,2)", "(1,2,3,4,6,5,7,8,10,9)", "(1,2,3,5,4,7,6,10,8,9)",
  "(1,10,8,6,3,2,9,7,5,4)", "(1,2,3,4,5,6,8,7,9,10)", "(1,8,3,5,2,9,10,7,4,6)",
  "(1,2,3,4,5,6,7,8,9,10)", "(1,10,9,7,8,6,5,4,3,2)", "(1,9,4,3,8,7,2,10,5,6)",
  "(1,10,2,9,8,5,6,7,4,3)", "(1,9,3,4,7,6,5,8,10,2)", "(1,5,9,8,2,6,10,4,3,7)",
  "(1,3,4,7,10)(2,5,6,8,9)", "(1,5,7,2,4,8,9,3,6,10)", "(1,3,10,2,4,6,8,5,7,9)",
  "(1,5,4,6,10,9)(2,7,3,8)", "(1,9,8,5,3,2,10,7,6,4)", "(1,10,9,8,7,6,5,4,2,3)",
  "(1,3,5,7,8,10,2,4,6,9)", "(1,4,6,10,3,7,9,2,5,8)", "(1,9,8,7,5,6,3,4,2,10)",
  "(1,2,3,4,5,6,7,8,9,10)", "(1,4,3,6,9,5,2,8,10,7)", "(1,2,6,5,10,9,7,4,3,8)",
  "(1,7,5,2,8,6,4,10,9,3)", "(1,3,4,6,7,5,8,9,10,2)", "(1,7,4,9,6,2,8,3,10,5)",
  "(1,2,6,7)(3,4,5,8,9,10)", "(1,10,9,8,7,6,5,4,2,3)", "(1,5,10,3,9,4,7,2,6)",
  "(1,10,9,8,7,6,5,4,3,2)", "(1,6,3,8,2,7,9,4,10,5)", "(1,9,3,4,7,8,2,10,6,5)",
  "(1,8,9,5,7,6,3,4,10,2)", "(1,5,10,4,9,3,8,2,7,6)", "(1,7,3,8,4,9,5,10,6)",
  "(1,3,7,2,6,9,4,8,10,5)", "(1,5,8,7,9,6,2,3,10,4)", "(1,4,9,2,7,10,5,8,3,6)",
  "(1,2,3,4,5,6,8,7,9,10)", "(1,7,8,4,10,6,2,3,9,5)", "(1,4,7,6,9,2)(3,10,8,5)",
  "(1,10,9,7,8,6,4,5,3,2)", "(1,3,9,5,10,4,7,6,8,2)", "(1,3,6,9,2,5,7,10,4,8)",
  "(1,2,7,8,3,4,9,10,6,5)", "(1,6,8,4,10,5,2,7,9,3)", "(1,3,10,2,4,7,8,5,6,9)",
  "(1,9,7,6,5,2,10,8,4,3)", "(1,9,8,5,2,10,7,6,3)", "(1,6,2,8,3,9,4,10,5)",
  "(1,3,7,5,4,6,8,2,9,10)", "(1,3,4,7,8,10,2,6,5,9)", "(1,4,7,10,3,5,9,2,6,8)",
  "(1,2,8,6,4,9,10,3,5,7)", "(1,10,6,3,9,7,4,2,8,5)", "(1,8,7,6,3,2)(4,10,5,9)",
  "(1,10,4,3,7,5,6,8,2,9)", "(1,6,2,7,3,9,4,8,10,5)", "(1,7,8,5,9,6,2,3,10,4)",
  "(1,9,6,3,2,8,7,4)(5,10)", "(1,5,6,10)(2,9,4,7,3,8)", "(1,6,10,3,7,2,5,8)(4,9)",
  "(1,6,5,8,2,10,4,7,3,9)", "(1,7,2,5,10,3,8,4,9,6)", "(1,2,3,5,4,7,6,8,10,9)",
  "(1,9,10,2,3,6,4,5,7,8)", "(1,3,5,2,4,6,8,10,7,9)", "(1,3,2,4,10,6,8,7,9,5)",
  "(1,4,3,10,7,6,9,8,5,2)", "(1,9,10,8,7,6,4,5,2,3)", "(1,4,7,10,3,6,8,2,5,9)",
  "(1,9,2,8,5,6,4,7,3,10)", "(1,8,10,2,4,6,3,5,7,9)", "(1,9,8,7,5,6,4,3,2,10)",
  "(1,10,6,3,9,7,4,2,8,5)", "(1,6,9,4,10,3,8,2,7)", "(1,10,9,8,7,6,5,3,4,2)",
  "(1,9,10,7,3,6,5,4,2,8)", "(1,2,5,8,10,4,3,9,6,7)", "(1,7,2,8,3,9,5,10,6)",
  "(1,10,9,8,7,6,5,4,3,2)", "(1,9,8,4,3,7,10,2,6,5)", "(1,2,3,4,5,6,7,8,9,10)",
  "(1,6,3,8,5,10,7,2,9,4)", "(1,7,3,4,10,6,2,8,9,5)", "(1,4,5,7,8,6,9,10,3,2)",
  "(1,10,9,2,3,6,5,4,7,8)", "(1,5,10,4,8,3,7,2,6)", "(1,9,10,8,2,5,3,6,4,7)",
  "(1,9,10,8,7,6,5,4,3,2)"
)

n_before_i <- function(p, i) {
  (i - 1) * p - (i - 1) * i / 2
}

get_number_of_an_edge <- function(p, i, j) {
  if (p < max(i, j)) {
    stop("Wrong i or j")
  }
  if (i >= j) {
    stop("i >= j")
  }

  n_before_i(p, i) + (j - i)
}

perm_to_coloring_partition <- function(perm) {
  vPartition <- unclass(permutations::as.cycle(perm))[[1]]
  all_non_in_order <- unlist(vPartition)
  for (i in setdiff(1:p, all_non_in_order)) {
    vPartition[[length(vPartition) + 1]] <- i
  }
  # now, vPartition is done

  where_vertex <- numeric(p)
  for (i in 1:length(vPartition)) {
    where_vertex[vPartition[[i]]] <- i
  }

  ePartition_numeric <- numeric(p * (p - 1) / 2)

  edge_done <- logical(p * (p - 1) / 2)
  edge_index <- 0

  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      number_of_an_edge <- get_number_of_an_edge(p, i, j)
      if (edge_done[number_of_an_edge]) {
        next
      }
      edge_index <- edge_index + 1

      partition_i <- vPartition[[where_vertex[i]]]
      partition_j <- vPartition[[where_vertex[j]]]

      now_i <- which(partition_i == i)
      start_i <- now_i
      now_j <- which(partition_j == j)
      start_j <- now_j

      this_i <- partition_i[now_i]
      this_j <- partition_j[now_j]

      now_number_of_an_edge <- get_number_of_an_edge(p, min(this_i, this_j), max(this_i, this_j))

      while (!edge_done[now_number_of_an_edge]) {
        ePartition_numeric[now_number_of_an_edge] <- edge_index
        edge_done[now_number_of_an_edge] <- TRUE

        now_i <- now_i + 1
        if (now_i > length(partition_i)) {
          now_i <- 1
        }
        now_j <- now_j + 1
        if (now_j > length(partition_j)) {
          now_j <- 1
        }

        this_i <- partition_i[now_i]
        this_j <- partition_j[now_j]

        now_number_of_an_edge <- get_number_of_an_edge(p, min(this_i, this_j), max(this_i, this_j))
      }
    }
  }

  ePartition_max <- max(ePartition_numeric)
  ePartition <- vector("list", ePartition_max)
  for (i in 1:ePartition_max) {
    ePartition[[i]] <- which(ePartition_numeric == i)
  }
  # now, ePartition is done

  new_ePartition <- lapply(ePartition, function(edges) {
    edges + p
  })

  factor_partition <- c(vPartition, new_ePartition)

  # now, factor_partition is done

  my_out <- numeric(p + p * (p - 1) / 2)
  for (i in 1:length(factor_partition)) {
    my_out[factor_partition[[i]]] <- i
  }

  my_out
}

coloring_partition_good <- perm_to_coloring_partition("(1,2,3,4,5,6,7,8,9,10)")
ARI <- my_perms %>% sapply(function(perm) {
  adjustedRandIndex(coloring_partition_good, perm_to_coloring_partition(perm))
})
sort(ARI)
