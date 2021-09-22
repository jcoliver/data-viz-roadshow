# Plot of soil chemistry and various microbes
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-09-22

library(dplyr)

# Renaming columns to be nicer.
envs <- envs %>%
  rename(Sample_Number = Sample.Number,
         Total_N = Total.N....,
         Total_C = Total.C....,
         Inorganic_C = Inorganic.C....,
         GPos_GNeg = Gram._Gram.,
         Per_AM_Fungi = Per_AM.Fungi,
         Per_Gram_Neg = Per_Gram.Negative,
         Per_Gram_Pos = Per_Gram.Positive)

envs <- envs %>%
  rename(Organic_C = Organic.Carbon....,
         Macro_Aggregates = Macro.Aggregates....,
         Sample_Ref = Sample.Ref)

envs <- envs %>%
  rename(Soil_Series = Soil.series)
colnames(envs)

preds <- c("Total_N", "Total_C", "Inorganic_C", "Organic_C", "Macro_Aggregates")
resps <- c("Per_Fungi", "Per_Eukaryote", "Per_Anaerobe", "Per_Actinomycetes")

pairs(envs[, c(preds, resps)])

pairs(envs[, c(preds, "Fungi_Bacteria", "Predator_Prey", "GPos_GNeg")])

# Possible plots (Y vs X):
# Per_Fungi vs. Macro_Aggregates (negative)
plot(x = envs$Macro_Aggregates, y = envs$Per_Fungi)
# Per_Actinomycetes vs. Organic_C (negative)


envs <- envs[!is.na(envs$Per_Actinomycetes), ]
plot(x = envs$Organic_C, y = envs$Per_Actinomycetes)
summary(lm(Per_Actinomycetes ~ Organic_C, data = envs))
plot(x = envs$Total_C, y = envs$Per_Actinomycetes)
summary(lm(Per_Actinomycetes ~ Total_C, data = envs))

set.seed(20210922)
for_table <- envs[sample(x = 1:nrow(envs), size = 15), c("Organic_C", "Per_Actinomycetes")]
for_table <- for_table[order(for_table$Organic_C), ]
for_table

rft <- c(70, 62, 9, 24, 54)
envs[rft, c("Organic_C", "Per_Actinomycetes")]
# Organic_C Per_Actinomycetes
# 70      0.10          45.48088
# 62      0.19          28.64416
# 9       0.48          24.40180
# 24      0.76          17.57020
# 54      0.89          19.79760

