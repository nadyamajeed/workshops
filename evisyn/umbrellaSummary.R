##### START OF CODE #####

# R version 4.3.1
options(scipen = 999)
library(dplyr)   # version 1.1.4
library(ggplot2) # version 3.5.0
library(ggforce) # version 0.4.2
library(ggpubr)  # version 0.6.0

# generate some artificial data
# for demonstration purposes
demodata =
  data.frame(
    meta_id = 1:40,
    yearMeta = sample(2000:2020, size = 40, replace = TRUE),
    yearStudyMin = sample(1970:1985, size = 40, replace = TRUE),
    yearStudyMax = sample(1986:2000, size = 40, replace = TRUE),
    N = round(scale(rbeta(40, 1, 500)) * 140 + 240),
    k = round(rnorm(n = 40, mean = 40, sd = 15)),
    recordType = sample(c("Journal article", "Journal article", "Journal article", "Thesis", "Conference"), size = 40, replace = TRUE),
    otherCharacteristic = sample(c("A", "B", "C"), size = 40, replace = TRUE)
  )

# create plot for year of meta-analysis
p1 = ggplot(demodata, aes(x = yearMeta)) +
  # histogram
  geom_histogram(binwidth = 1) +
  # fix axes and main plot area
  scale_y_continuous(breaks = 0:40) +
  scale_x_continuous(breaks = 2000:2030) +
  xlab("Year meta-analysis was made available") +
  ylab("Number of meta-analyses\nin that year") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 5)) +
  # add title of plot
  ggtitle("Year of meta-analysis")

# create plot for year of studies
p2 = ggplot(
  demodata %>%
    dplyr::arrange(yearStudyMin, yearStudyMax) %>%
    dplyr::mutate(order = row.names(.) %>% as.numeric()),
  aes(y = rev(order))) +
  # 'caterpillar' style plot
  geom_errorbarh(aes(xmin = yearStudyMin, xmax = yearStudyMax)) +
  # fix axes and main plot area
  scale_x_continuous(
    breaks = seq(1970, 2000, 5),
    limits = c(1970, 2000),
    minor_breaks = seq(1970, 2000, 1)) +
  xlab("Year of included studies") +
  ylab("Each row corresponds\nto one meta-analysis") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  # add title of plot
  ggtitle("Year of studies in meta-analysis")

# create plot for sample size
p3 = ggplot(
  demodata %>%
    dplyr::mutate(temp = 1),
  aes(y = N, x = temp)) +
  # violin and box plot
  geom_violin() +
  geom_boxplot(width = 0.1) +
  # fix axes
  scale_y_continuous(breaks = function(x) pretty(x, n = 10)) +
  xlab("Density") +
  ylab("Sample size\nin meta-analysis") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  # add title of plot
  ggtitle("Violin and box plot of\nsample size in meta-analysis")

# create plot for number of studies
p4 = ggplot(
  demodata %>%
    dplyr::mutate(temp = 1),
  aes(y = k, x = temp)) +
  # violin and box plot
  geom_violin() +
  geom_boxplot(width = 0.1) +
  # fix axes
  scale_y_continuous(breaks = function(x) pretty(x, n = 10)) +
  xlab("Density") +
  ylab("Number of studies\nin meta-analysis") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  # add title of plot
  ggtitle("Violin and box plot of\nnumber of studies in meta-analysis")

# create plot for breakdown of record types
p5 = ggplot(demodata, aes(x = recordType)) +
  # histogram
  geom_histogram(stat = "count") +
  # fix axes
  scale_y_continuous(limits = c(0, 40)) +
  xlab("Type of record") +
  ylab("Number of meta-analyses") +
  theme(axis.text.x = element_text(size = 8)) +
  # add title of plot
  ggtitle("Breakdown of record types")

# create plot for breakdown of other characteristic
p6 = ggplot(demodata, aes(x = otherCharacteristic)) +
  # histogram
  geom_histogram(stat = "count") +
  # fix axes
  scale_y_continuous(limits = c(0, 40)) +
  xlab("Characteristic") +
  ylab("Number of meta-analyses") +
  # add title of plot
  ggtitle("Breakdown of characteristic")

# combine all into one plot
ggarrange(
  p1, p2,
  p3, p4,
  p5, p6,
  labels = c("A", "B", "C", "D", "E", "F"),
  ncol = 2, nrow = 3
)

# save as pdf
ggsave("combinedPlot.pdf", width = 10, height = 10)

##### END OF CODE #####
