simulator = function(
    ##### SET INPUT PARAMETERS -----
    N = 1000, 
    true_change = 1, 
    true_variance = 1, 
    error_variance = 1) {
  ##### SIMULATE DATA -----
  simData =
    data.frame(ID = 1:N) |>
    dplyr::mutate(
      # true levels at t1 and t2
      eta_t1 = rnorm(n = N, mean = 0, sd = sqrt(true_variance)),
      eta_t2 = eta_t1 + rnorm(n = N, mean = true_change, sd = sqrt(true_variance)),
      # error at t1 and t2
      epsilon_t1 = rnorm(n = N, mean = 0, sd = sqrt(error_variance)),
      epsilon_t2 = rnorm(n = N, mean = 0, sd = sqrt(error_variance)),
      # observed levels at t1 and t2
      y_t1 = eta_t1 + epsilon_t1,
      y_t2 = eta_t2 + epsilon_t2)
  ##### GET OUTPUTS -----
  observed_change = with(simData, mean(y_t2)-mean(y_t1))
  observed_d_and_ci = effectsize::cohens_d(x = "y_t1", y = "y_t2", data = simData, paired = TRUE)
  return(
    observed_d_and_ci |> 
      as.data.frame() |> 
      dplyr::mutate(
        CI = NULL,
        Mdiff = observed_change,
        N = N, 
        true_change = true_change, 
        true_variance = true_variance, 
        error_variance = error_variance))
}

library(ggplot2)

# low error
ggplot(
  sapply(
    X = rep(seq(from = 10, to = 1000, by = 5), 5),
    FUN = function(X) simulator(N = X, error_variance = 0.1)) |>
    as.data.frame() |>
    t() |>
    as.data.frame() |>
    dplyr::mutate_all(as.numeric), 
  aes(x = N, y = Mdiff)) +
  geom_point() +
  geom_line(stat = "smooth", method = "lm", se = FALSE, colour = "red") +
  geom_hline(yintercept = 1)

# medium error
ggplot(
  sapply(
    X = rep(seq(from = 10, to = 1000, by = 5), 5),
    FUN = function(X) simulator(N = X, error_variance = 0.5)) |>
    as.data.frame() |>
    t() |>
    as.data.frame() |>
    dplyr::mutate_all(as.numeric), 
  aes(x = N, y = Mdiff)) +
  geom_point() +
  geom_line(stat = "smooth", method = "lm", se = FALSE, colour = "red") +
  geom_hline(yintercept = 1)

# high error
ggplot(
  sapply(
    X = rep(seq(from = 10, to = 1000, by = 5), 5),
    FUN = function(X) simulator(N = X, error_variance = 1)) |>
    as.data.frame() |>
    t() |>
    as.data.frame() |>
    dplyr::mutate_all(as.numeric), 
  aes(x = N, y = Mdiff)) +
  geom_point() +
  geom_line(stat = "smooth", method = "lm", se = FALSE, colour = "red") +
  geom_hline(yintercept = 1)

# very high error
ggplot(
  sapply(
    X = rep(seq(from = 10, to = 1000, by = 5), 5),
    FUN = function(X) simulator(N = X, error_variance = 5)) |>
    as.data.frame() |>
    t() |>
    as.data.frame() |>
    dplyr::mutate_all(as.numeric), 
  aes(x = N, y = Mdiff)) +
  geom_point() +
  geom_line(stat = "smooth", method = "lm", se = FALSE, colour = "red") +
  geom_hline(yintercept = 1)
