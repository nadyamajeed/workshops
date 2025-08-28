# read in data
dataPasta = read.csv("https://raw.githubusercontent.com/nadyamajeed/workshops/refs/heads/main/regression/2025/data/dataSimPasta.csv")

# assuming pastaLiking is on x-axis
# and happinessScore is on y-axis

# get x-ticks and y-ticks
y_ticks_lo = floor(min(dataPasta$happinessScore))
y_ticks_hi = ceiling(max(dataPasta$happinessScore))
x_ticks_lo = floor(min(dataPasta$pastaLiking))
x_ticks_hi = ceiling(max(dataPasta$pastaLiking))

# get y-position of pastaLiking violin & box
y_pos = y_ticks_lo - 0.5

# get x-position of happinessScore violin & box
x_pos = x_ticks_lo - 0.5

# get x-limits and y-limits of plot
y_lim_lo = y_pos - 0.5
y_lim_hi = y_ticks_hi + 0.5
x_lim_lo = x_pos - 0.5
x_lim_hi = x_ticks_hi + 0.5

# plot
# should only need to change at the three "change here" spots
ggplot(
  dataPasta,
  aes(x = pastaLiking, y = happinessScore) # <-- change here
) +
  geom_smooth(method = "lm") +
  geom_point(size = 0.05, alpha = 0.5) +
  geom_violin(aes(y = y_pos), width = 0.5, fill = "lightblue") +
  geom_boxplot(aes(y = y_pos), width = 0.25) +
  geom_violin(aes(x = x_pos), width = 0.5, fill = "lightblue") +
  geom_boxplot(aes(x = x_pos), width = 0.25) +
  scale_y_continuous(
    "Happiness", # <-- change here
    limits = c(y_lim_lo, y_lim_hi), 
    breaks = y_ticks_lo:y_ticks_hi) +
  scale_x_continuous(
    "Pasta Liking", # <-- change here
    limits = c(x_lim_lo, x_lim_hi), 
    breaks = x_ticks_lo:x_ticks_hi) +
  theme_minimal()

# can use ggsave() to save plot if needed
