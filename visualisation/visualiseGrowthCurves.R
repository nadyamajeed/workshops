##### start of code #####
# quick visualisation of growth curves

library(dplyr)
library(ggplot2)
library(ggpubr)

##### values (CHANGE YOUR VALUES HERE) #####

# time
time_start = 0
time_end = 13

# linear model
# only i (intercept) and s (linear slope)

lin_i = 0.787
lin_s = 0.316

# quadratic model
# i (intercept), s (linear slope), s2 (change in slope over time)

qua_i = 0.692
qua_s = 0.590
qua_s2 = -0.075

##### prepare helper data (DO NOT EDIT) #####

time = time_start:time_end

lin_data = data.frame(time = time) %>%
  mutate(point = lin_i + lin_s*time)

qua_data = data.frame(time = time) %>%
  mutate(point = qua_i + qua_s*time + qua_s2*time*time)

##### plots (DO NOT EDIT) #####

plot_general = ggplot() +
  scale_x_continuous(
    lim = c(time_start, time_end), 
    breaks = time_start:time_end) +
  ylab("outcome")

plot_lin = plot_general +
  geom_point(data = lin_data, mapping = aes(x = time, y = point))

plot_qua = plot_general +
  geom_point(data = qua_data, mapping = aes(x = time, y = point))

ggarrange(
  plot_lin, plot_qua,
  labels = c("Linear model", "Quadratic model"),
  vjust = 1.25
)

##### end of code #####
