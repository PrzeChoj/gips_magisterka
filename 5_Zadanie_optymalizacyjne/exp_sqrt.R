p <- 4
S <- matrix(
  data = c(
    1.1, 0.8, 0.6, 0.2,
    0.8, 2.1, 0.8, 0.9,
    0.6, 0.8, 1.1, 0.8,
    0.2, 0.9, 0.8, 2.1
  ),
  nrow = p, byrow = TRUE
)

exp1 <- plot(gips(S, 10, perm = "(1,2,3,4)"), type = 'heatmap') +
  ggplot2::labs(title = "Przestrzeń pokolorowana permutacją (1,2,3,4)", x = "", y = "", fill = "value") +
  ggplot2::theme(legend.position = "none")
ggplot2::ggsave(
  file.path(".", "plots", "exp_sqrt_1.png"),
  exp1,
  width = 12.4, height = 12.6,
  units = "cm"
)

exp2 <- plot(gips(S, 10, perm = "(1,3)(2,4)"), type = 'heatmap') +
  ggplot2::labs(title = "Przestrzeń pokolorowana permutacją (1,3)(2,4)", x = "", y = "", fill = "value") +
  ggplot2::theme(legend.position = "none")
ggplot2::ggsave(
  file.path(".", "plots", "exp_sqrt_2.png"),
  exp2,
  width = 12.4, height = 12.6,
  units = "cm"
)
