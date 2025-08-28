# ─────────────────────────────────────────────────────────────────────────────
# Load & prepare data
# ─────────────────────────────────────────────────────────────────────────────

# Read a demo dataset directly from GitHub.
# The file has two numeric columns used below:
#   - pastaLiking      (predictor; x-axis)
#   - happinessScore   (outcome;   y-axis)
dataPasta = read.csv(
  "https://raw.githubusercontent.com/nadyamajeed/workshops/refs/heads/main/regression/2025/data/dataSimPasta.csv"
)

# ─────────────────────────────────────────────────────────────────────────────
# Compute axis breaks and plot margins
# ─────────────────────────────────────────────────────────────────────────────

# Choose tidy tick marks for y by rounding the observed min/max outward.
y_ticks_lo = floor(min(dataPasta$happinessScore))
y_ticks_hi = ceiling(max(dataPasta$happinessScore))

# Choose tidy tick marks for x by rounding the observed min/max outward.
x_ticks_lo = floor(min(dataPasta$pastaLiking))
x_ticks_hi = ceiling(max(dataPasta$pastaLiking))

# ─────────────────────────────────────────────────────────────────────────────
# Positions for marginal distributions
# (We’ll draw *horizontal* violins/box for x, and *vertical* violins/box for y.)
# These are placed just outside the main plotting area using constant aesthetics.
# ─────────────────────────────────────────────────────────────────────────────

# Fixed y-position where the *x-variable’s* (pastaLiking) marginal violin/box will sit.
y_pos = y_ticks_lo - 0.5

# Fixed x-position where the *y-variable’s* (happinessScore) marginal violin/box will sit.
x_pos = x_ticks_lo - 0.5

# Expand plot limits so the marginal violins/boxes are visible (not clipped).
y_lim_lo = y_pos - 0.5
y_lim_hi = y_ticks_hi + 0.5
x_lim_lo = x_pos - 0.5
x_lim_hi = x_ticks_hi + 0.5

# ─────────────────────────────────────────────────────────────────────────────
# Plot: scatter + OLS smooth + marginal distributions
# Only the three “change here” labels need updating if you swap variables.
# ─────────────────────────────────────────────────────────────────────────────

ggplot(
  dataPasta,
  aes(x = pastaLiking, y = happinessScore) # <-- change here
) +
  # Fitted linear trend (ordinary least squares)
  geom_smooth(method = "lm") +
  # Raw data points (tiny, semi-transparent to manage overplotting)
  geom_point(size = 0.05, alpha = 0.5) +
  # Marginal distribution of the *x-variable* shown along the bottom:
  # map y to a constant (y_pos) so the violin/box sit below the x-axis.
  geom_violin(aes(y = y_pos), width = 0.5, fill = "lightblue") +
  geom_boxplot(aes(y = y_pos), width = 0.25) +
  # Marginal distribution of the *y-variable* shown at the left:
  # map x to a constant (x_pos) so the violin/box sit left of the y-axis.
  geom_violin(aes(x = x_pos), width = 0.5, fill = "lightblue") +
  geom_boxplot(aes(x = x_pos), width = 0.25) +
  # Y-axis label, limits (to reveal bottom marginal), and integer tick marks
  scale_y_continuous(
    "Happiness",                    # <-- change here (y-axis label)
    limits = c(y_lim_lo, y_lim_hi),
    breaks = y_ticks_lo:y_ticks_hi
  ) +
  # X-axis label, limits (to reveal left marginal), and integer tick marks
  scale_x_continuous(
    "Pasta Liking",                 # <-- change here (x-axis label)
    limits = c(x_lim_lo, x_lim_hi),
    breaks = x_ticks_lo:x_ticks_hi
  ) +
  # Minimal theme for a clean look
  theme_minimal()

# Tip: To save, call e.g.:
# ggsave("pasta_vs_happiness.pdf", width = 6, height = 6)
