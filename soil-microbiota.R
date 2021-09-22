# Plot of soil chemistry and various microbes
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-09-22

library(dplyr)
library(ggplot2)

# envs <- read.csv(file = "~/Desktop/joined_sheets.csv")
# envs <- envs %>%
#   rename(Soil_Series = Soil.series,
#          Sample_Number = Sample.Number,
#          Total_N = Total.N....,
#          Total_C = Total.C....,
#          Inorganic_C = Inorganic.C....,
#          Organic_C = Organic.Carbon....,
#          Macro_Aggregates = Macro.Aggregates....,
#          Sample_Ref = Sample.Ref,
#          GramPos_GramNeg = Gram._Gram.,
#          Per_AM_Fungi = Per_AM.Fungi,
#          Per_Gram_Neg = Per_Gram.Negative,
#          Per_Gram_Pos = Per_Gram.Positive)

# write.csv(file = "data/Soil_Micro_Data.csv", x = envs, row.names = FALSE)
envs <- read.csv(file = "data/Soil_Micro_Data.csv")

envs$Site <- factor(x = trimws(envs$Site))
envs$Depth <- factor(x = trimws(envs$Depth))
envs$Soil_Series <- factor(x = trimws(envs$Soil_Series))
envs$Irrigation <- factor(x = trimws(envs$Irrigation))
summary(envs[, c("Site", "Depth", "Soil_Series", "Irrigation")])

# Plot for slideshow
boxplot(Per_Fungi ~ Soil_Series, data = envs)
t.test(x = envs$Per_Fungi[envs$Soil_Series == "Casa Grande"], 
       y = envs$Per_Fungi[envs$Soil_Series == "Mohall"])

# Get summary stats (mean + SE) for table
soil_summary <- envs %>%
  group_by(Soil_Series) %>%
  summarize(mean_fungi = mean(Per_Fungi, na.rm = TRUE),
            se_fungi = sd(Per_Fungi, na.rm = TRUE)/sqrt(n()))

# Plot boxplot
soil_boxplot <- ggplot(data = envs, mapping = aes(x = Soil_Series, 
                                                  y = Per_Fungi)) +
  geom_boxplot() +
  xlab(label = "Soil Series") +
  ylab(label = "% Fungi") +
  theme_bw()
soil_boxplot
ggsave(file = "output/soil-fungi.png", plot = soil_boxplot)

# Possible plots for notebook (Y vs X):
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
