# Plots comparing Anscombe data
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-02-23

################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)

# Some crazy wrangling to get into a ggplot-amenable form
anscombe_long <- anscombe %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(cols = -row_num) %>%
  mutate(plot_num = substr(name, start = 2, stop = 2),
         axis_id = substr(name, start = 1, stop = 1)) %>%
  mutate(plot_row = paste0(plot_num, "-", row_num)) %>%
  pivot_wider(id_cols = plot_row, 
              names_from = axis_id, 
              values_from = value) %>%
  mutate(plot_num = substr(plot_row, start = 1, stop = 1))

head(anscombe_long)

# Make a plot with just the regression lines
anscombe_lines <- ggplot(data = anscombe_long, 
                         mapping = aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(. ~ plot_num) + 
  ylim(2.5, 13.0) +
  theme_bw()
print(anscombe_lines)
ggsave(filename = "output/anscombe-lines.png", plot = anscombe_lines)

# Make a plot that includes regression lines and data
anscombe_points <- ggplot(data = anscombe_long, 
                         mapping = aes(x = x, y = y)) +
  geom_point(mapping = aes(color = plot_num), size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(. ~ plot_num) +
  ylim(2.5, 13.0) +
  theme_bw() +
  theme(legend.position = "none")
print(anscombe_points)
ggsave(filename = "output/anscombe-points.png", plot = anscombe_points)
