# Visualizing concrete strength data
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-02-17

################################################################################
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)

# Read in concrete data
concrete <- read.csv(file = "data/Concrete_Data.csv")

# Plot strength vs. age, using OLS linear regression
ggplot(data = concrete, mapping = aes(x = Age, 
                                      y = Compressive_Strength)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm")

# Plot strength vs. age, adding local regression line
ggplot(data = concrete, mapping = aes(x = Age, 
                                      y = Compressive_Strength)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess")

# Add amount of cement as color of points
ggplot(data = concrete, mapping = aes(x = Age, 
                                      y = Compressive_Strength,
                                      color = Cement)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess")
