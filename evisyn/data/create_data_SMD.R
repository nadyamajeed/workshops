##### GENERATE DATA FOR SIMPLE (TWO-LEVEL) META-ANALYSIS OF SMD #####

# R version 4.4.0
set.seed(0)

# set up some changeable parameters
no_of_samples = 30
n_min = 25
n_max = 100
ndiff_max = 10

# create single simulated dataset
# of a meta-analysis of SMDs

# group 1
n1 = sample(n_min:n_max, size = no_of_samples, replace = TRUE)
m1 = rnorm(n = no_of_samples, mean = 5, sd = 1) |> round(2)
sd1 = 
  (sample((0.5*n_max):(1.25*n_max), size = no_of_samples, replace = TRUE) /
     n1) |> 
  round(2)

# group 2
n2 = n1 + sample(-ndiff_max:ndiff_max, size = no_of_samples, replace = TRUE)
m2 = m1 - rnorm(n = no_of_samples, mean = 4, sd = 0.5) |> round(2)
sd2 = 
  (sample((0.5*n_max):(1.25*n_max), size = no_of_samples, replace = TRUE) /
     n2) |> 
  round(2)

# combine
data_recorded_A = data.frame(
  sample = 1:no_of_samples,
  n1, m1, sd1,
  n2, m2, sd2
)

# edit data to include country effect
data_recorded_A$country = sample(c("A", "A", "B", "C"), size = no_of_samples, replace = TRUE)
data_recorded_A$m1 = with(data_recorded_A, ifelse(country == "B", m1 + 1, m1))

##### GENERATE DATA FOR THREE-LEVEL META-ANALYSIS OF SMD #####

# create second dataset
# which is based on (i.e., correlated with) the first.
# this will be our second effect size per sample.

# duplicate the data
data_recorded_B = data_recorded_A

# change the means slightly
data_recorded_B$m1 = (data_recorded_B$m1 + rnorm(no_of_samples)) |> round(2)
data_recorded_B$m2 = (data_recorded_B$m2 + rnorm(no_of_samples)) |> round(2)

# change the SDs slightly
data_recorded_B$sd1 = (data_recorded_B$sd1 + rnorm(no_of_samples, sd = 0.1)) |> round(2)
data_recorded_B$sd2 = (data_recorded_B$sd2 + rnorm(no_of_samples, sd = 0.1)) |> round(2)

# combine the data
data_recorded_all = rbind(data_recorded_A, data_recorded_B)
data_recorded_all = data_recorded_all[order(data_recorded_all$sample), ]

##### WRITE BOTH DATASETS #####

write.csv(data_recorded_A,  "data_simple_SMD.csv", row.names = FALSE)
write.csv(data_recorded_all, "data_multi_SMD.csv", row.names = FALSE)

##### END OF CODE #####
