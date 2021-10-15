# Boxplot on plywood data
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-02-23

################################################################################
library(dplyr)
library(ggplot2)

# Plywood strength data from https://data.mendeley.com/datasets/hkgstm9sxg/1
plywood <- read.csv(file = "data/Plywood_Data.csv")

# For our purposes, only want those with internal fiber
fiber_internal <- plywood %>%
  filter(position == "internal")

# Get mean and SE for two types
plywood_summary <- fiber_internal %>%
  group_by(fiber) %>%
  summarize(mean_IB = mean(IB),
            se_IB = sd(IB)/sqrt(n()))
plywood_summary

# Plot the data
plywood_plot <- ggplot(data = fiber_internal, 
                       mapping = aes(x = fiber, y = IB)) +
  geom_boxplot() +
  ylab(label = "Internal bond (MPa)") +
  xlab(label = "Orientation") +
  scale_x_discrete(labels = c("Cross" = "Perpendicular",
                              "Parallel" = "Parallel")) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18))
print(plywood_plot)

ggsave(filename = "output/boxplot-plywood.png", plot = plywood_plot)
