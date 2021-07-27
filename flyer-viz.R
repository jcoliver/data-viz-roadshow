# data viz
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-27

library(ggplot2)
library(dplyr)

# Do a brownian motion
gens <- 100
start <- 0
runs <- 50
special_run <- sample(x = 1:runs, size = 1)

for (i in 1:runs) {
  if (i == 1) {
    data <- NULL
  }
  one_run <- data.frame(run = i,
                        generations = 1:gens,
                        value = NA)
  one_run$special <- dplyr::if_else(i == special_run, true = TRUE, false = FALSE)
  # We want one run to have positive trend
  if (i == special_run) {
    start <- -20
  } else {
    start <- runif(n = 1, min = -20, max = 20)
  }
  
  one_run$value[1] <- start
  for (j in 2:gens) {
    if (i == special_run) {
      one_run$value[j] <- one_run$value[j - 1] + rnorm(n = 1, mean = 0.5)
    } else {
      one_run$value[j] <- one_run$value[j - 1] + rnorm(n = 1)
    }
  }
  if (is.null(data)) {
    data <- one_run
  } else {
    data <- data %>%
      bind_rows(one_run)
  }
}


ggplot(data = data, mapping = aes(x = generations, y = value, color = run)) +
  geom_line(alpha = 0.75) +
  theme_minimal()

grey_lines <- ggplot(data = data, mapping = aes(x = generations, 
                                                y = value, 
                                                group = run,
                                                color = special)) +
  geom_line(size = 1) +
  # Setting different transparency for grey lines (77) and red line (CC)
  scale_color_manual(values = c("#88888844", "#8B0015FF")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none")
grey_lines
ggsave(filename = "output/flyer-grey.svg", grey_lines)

# Another brownian motion, coloring lines by where they end up

gens <- 500
start <- 0
runs <- 100
for (i in 1:runs) {
  if (i == 1) {
    data <- NULL
  }
  one_run <- data.frame(run = i,
                        generations = 1:gens,
                        value = NA)
  one_run$value[1] <- start
  for (j in 2:gens) {
    one_run$value[j] <- one_run$value[j - 1] + rnorm(n = 1)
  }
  # Need to add a column with ending value to use for coloring
  one_run$end <- one_run$value[gens]
  if (is.null(data)) {
    data <- one_run
  } else {
    data <- data %>%
      bind_rows(one_run)
  }
}

colored_lines <- ggplot(data = data, mapping = aes(x = generations, 
                                                y = value, 
                                                group = run,
                                                color = end)) +
  geom_line(alpha = 0.1, size = 0.5) +
  # Setting different transparency for grey lines (77) and red line (CC)
  # scale_color_manual(values = c("#88888877", "#8B0015AA")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none")
colored_lines
ggsave(filename = "output/flyer-colors.svg", colored_lines)
