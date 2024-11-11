# START OF CODE #

library(metafor) # 4.6-0

# create 10 rows of fake data
n = 10

# create data such that each row has:
# - prevalence in % as RECORDED_prev
# - total sample size as RECORDED_N
# UNSEEN_count is for simulation purposes,
# usually not known during data extraction
simPrevalenceRaw = 
  data.frame(
    id = 1:n,
    RECORDED_N = rnorm(n, mean = 200, sd = 50) |> round(0)
  ) |>
  dplyr::mutate(RECORDED_N = ifelse(RECORDED_N < 50, 50, RECORDED_N)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(UNSEEN_count = sample(x = 1:RECORDED_N, size = 1)) %>%
  dplyr::ungroup() |>
  dplyr::mutate(RECORDED_prev = round(UNSEEN_count/RECORDED_N * 100, 2)) |>
  dplyr::select(id, RECORDED_prev, RECORDED_N, UNSEEN_count)

# clean data so that prevalence % is converted to 
# xi, the number of individuals experiencing the event/diagnosis
# and RECORDED_N is used for 
# ni, the total number of individuals assessed
simPrevalenceClean = 
  metafor::escalc(
    data = simPrevalenceRaw |> dplyr::mutate(
      ni = RECORDED_N,
      xi = round(RECORDED_N * RECORDED_prev/100)
    ),
    xi = xi,
    ni = ni,
    measure = "PR" # <- there are other options, pls read metafor::escalc documentation section 3
  )

# conduct meta-analysis
metafor::rma(
  yi = yi, vi = vi,
  data = simPrevalenceClean
)

# END OF CODE #
