########## START OF CODE ##########

# R version 4.4.1
library(dplyr)       # version 1.1.4
library(psych)       # version 2.5.3
library(ggplot2)     # version 3.5.2
library(ggstatsplot) # version 0.13.1
library(rlang)       # version 1.1.6

# display settings
options(scipen = 9999, digits = 4)

########## create artificial data for demonstration ##########
# simulated data pattern is that
# cat and lizard people (than dog people)
# and female people (than male people)
# are more likely to be happy
# and favourite pasta has no relationship to happiness

dataSim = dplyr::bind_rows(
  # happy people
  data.frame(
    sex = sample(
      x = c("M", "F", "F"),
      size = 200,
      replace = TRUE),
    pet = sample(
      x = c("Dog", "Cat", "Cat", "Cat", "Lizard", "Lizard"),
      size = 200,
      replace = TRUE),
    favourite_pasta = sample(
      x = c("Tagliatelle", "Farfalle", "Conchiglie"),
      size = 200,
      replace = TRUE),
    happiness = "Happy"
  ),
  # neutral people
  data.frame(
    sex = sample(
      x = c("M", "F"),
      size = 200,
      replace = TRUE),
    pet = sample(
      x = c("Dog", "Cat", "Lizard"),
      size = 200,
      replace = TRUE),
    favourite_pasta = sample(
      x = c("Tagliatelle", "Farfalle", "Conchiglie"),
      size = 200,
      replace = TRUE),
    happiness = "Neutral"
  ),
  # sad people
  data.frame(
    sex = sample(
      x = c("M", "M", "F"),
      size = 200,
      replace = TRUE),
    pet = sample(
      x = c("Dog", "Dog", "Cat", "Lizard"),
      size = 200,
      replace = TRUE),
    favourite_pasta = sample(
      x = c("Tagliatelle", "Farfalle", "Conchiglie"),
      size = 200,
      replace = TRUE),
    happiness = "Sad"
  )
) %>%
  # fix factor levels
  dplyr::mutate(
    favourite_pasta = factor(
      favourite_pasta,
      levels = c("Tagliatelle", "Farfalle", "Conchiglie")
    )
  )

########## histograms ##########

# sex
ggplot(dataSim, aes(x = sex)) +
  geom_histogram(stat = "count")

# pet
ggplot(dataSim, aes(x = pet)) +
  geom_histogram(stat = "count")

# relationship
ggplot(dataSim, aes(x = favourite_pasta)) +
  geom_histogram(stat = "count")

# happiness
ggplot(dataSim, aes(x = happiness)) +
  geom_histogram(stat = "count")

########## testing group differences ##########

getAllOutputs = function(current_variable, simulate.p.value = FALSE) {
  # select columns of interest
  data_current = dataSim %>%
    dplyr::select(happiness, all_of(current_variable))
  # create contingency table
  contingency_table = table(data_current)
  # fisher's test
  fishers_test = fisher.test(contingency_table, simulate.p.value = simulate.p.value)
  pval = fishers_test$p.value
  # combine plot and statistical test with ggbarstats
  plot = ggstatsplot::ggbarstats(
    # pass in data
    data_current,
    # pass in variables
    happiness, !!rlang::sym(current_variable),
    # display settings
    results.subtitle = FALSE,
    subtitle = paste0(
      "Fisher's exact test, p-value",
      ifelse(
        pval < 0.001,
        " < 0.001",
        paste0(" = ", round(pval, 3)))
    )
  ) +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    ) +
    guides(
      fill = guide_legend(
        title = "Happiness",
        reverse = FALSE)) +
    labs(
      x = paste(sapply(strsplit(gsub("_", " ", current_variable), " ")[[1]], function(word) {
        paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word))))
      }), collapse = " "),
      y = "Proportion of Subgroup"
    )
  # return all objects
  return(list(
    raw_data = data_current,
    contingency_table = contingency_table,
    fishers_test = fishers_test,
    plot = plot
  ))
}

#> sex -----
getAllOutputs("sex")

#> pet -----
getAllOutputs("pet", simulate.p.value = TRUE)

#> favourite pasta -----
getAllOutputs("favourite_pasta", simulate.p.value = TRUE)

########## END OF CODE ##########
