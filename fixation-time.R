# Plot of time to fixation for advantageous allele
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-14

library(dplyr)
# library(tidyr)
library(ggplot2)

fix_time <- function(N, s) {
  return((2 * log(x = 2 * N - 1) ) / s)
}

N_vector <- c(10, 100, 1000, 10000)
s_vector <- seq(0.005, by = 0.0005, length = 50)

df <- NULL
for (i in 1:length(N_vector)) {
  f_t <- fix_time(N = N_vector[i], s_vector)
  f_t_df <- data.frame(N = N_vector[i],
                       s = s_vector,
                       ftime = f_t)
  if (is.null(df)) {
    df <- f_t_df
  } else {
    df <- df %>%
      bind_rows(f_t_df)
  }
}

df$N_fac <- factor(df$N)

t_plot <- ggplot(data = df,
                 mapping = aes(x = s, 
                               y = ftime, 
                               color = N_fac,
                               group = N_fac)) +
  geom_line(size = 1) +
  xlab(label = expression(italic("s"))) +
  ylab(label = expression(italic("E(t)"))) +
  scale_color_discrete(name = expression(italic(N[e]))) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = c(0.8, 0.7),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
t_plot

ggsave(filename = "output/fixation-time-boxplot.png", 
       plot = t_plot)
