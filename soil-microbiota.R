# Plot of soil chemistry and various microbes
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-09-22

library(dplyr)

envs <- read.csv(file = "data/Soil_Micro_Data.csv")

# Possible plots (Y vs X):
# Per_Fungi vs. Macro_Aggregates (negative)
plot(x = envs$Macro_Aggregates, y = envs$Per_Fungi)
# Per_Actinomycetes vs. Organic_C (negative)
plot(x = envs$Organic_C, y = envs$Per_Actinomycetes)
summary(lm(Per_Actinomycetes ~ Organic_C, data = envs))
plot(x = envs$Total_C, y = envs$Per_Actinomycetes)
summary(lm(Per_Actinomycetes ~ Total_C, data = envs))

rows_for_table <- c(70, 62, 9, 24, 54)
envs[rows_for_table, c("Organic_C", "Per_Actinomycetes")]
# Organic_C Per_Actinomycetes
# 70      0.10          45.48088
# 62      0.19          28.64416
# 9       0.48          24.40180
# 24      0.76          17.57020
# 54      0.89          19.79760

# Testing polynomial models
one <- lm(Per_Actinomycetes ~ Total_C, data = envs)
two <- lm(Per_Actinomycetes ~ poly(x = Total_C, degree = 2), data = envs)
three <- lm(Per_Actinomycetes ~ poly(x = Total_C, degree = 3), data = envs)
four <- lm(Per_Actinomycetes ~ poly(x = Total_C, degree = 4), data = envs)
anova(one, two)
anova(two, three)
anova(three, four)
