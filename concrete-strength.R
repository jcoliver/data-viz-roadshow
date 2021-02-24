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

# Some EDA, looking for good relationships
pairs(concrete)

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

# What about a polynomial relationship?
ggplot(data = concrete, mapping = aes(x = Age, 
                                      y = Compressive_Strength,
                                      color = Cement)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 4))


single <- lm(Compressive_Strength ~ Age, data = concrete)
order_2 <- lm(Compressive_Strength ~ poly(Age, 2), data = concrete)
order_3 <- lm(Compressive_Strength ~ poly(Age, 3), data = concrete)
# Stop at order 4
order_4 <- lm(Compressive_Strength ~ poly(Age, 4), data = concrete)
order_5 <- lm(Compressive_Strength ~ poly(Age, 5), data = concrete)

anova(order_3, order_4)
anova(order_4, order_5)
