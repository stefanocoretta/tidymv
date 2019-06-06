#### Poisson dataset ####
set.seed(9753)
obs <- 25
x_i <- 50
y_a <- NULL
for (i in seq(1, 5, length.out = x_i)) {
  y_a <- c(y_a, rpois(obs, log(i) ^ exp(0.6210526)))
}
y_b <- NULL
for (i in seq(1, 5, length.out = x_i)) {
  y_b <- c(y_b, rpois(obs, log(i) ^ exp(0.1736842)))
}

pois_df <- tibble(
  y = c(y_a, y_b),
  x = c(rep(1:x_i, each = obs), rep(1:x_i, each = obs)),
  fac = as.factor(rep(c("a", "b"), each = obs * 50))
)

usethis::use_data(pois_df)
