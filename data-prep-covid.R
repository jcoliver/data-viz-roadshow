# Preparing covid data for plotting
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-14

library(dplyr)
library(tidyr)

# Data are tidied up from download from 
# https://covid.cdc.gov/covid-data-tracker/
# on 2021-07-14

cases <- read.csv(file = "data/COVID_Case_Data.csv",
                  na.strings = c("NA", "N/A"))

vacs <- read.csv(file = "data/COVID_Vaccination_Data.csv",
                 na.strings = c("NA", "N/A"))

# Join vaccination and case data and select columns of interest
disease_data <- cases %>%
  inner_join(vacs, by = c("State.Territory" = "State.Territory.Federal.Entity")) %>%
  rename("State" = "State.Territory",
         "Case_Rate" = "Case.Rate.per.100000",
         "Dose_Rate" = "Doses.Delivered.per.100K",
         "One_Dose" = "Percent.of.Total.Pop.with.at.least.One.Dose.by.State.of.Residence") %>%
  select(State, Case_Rate, Dose_Rate, One_Dose) %>%
  drop_na()

# Write to a file we can use for plotting
write.csv(x = disease_data,
          file = "data/COVID_Data.csv",
          row.names = FALSE)

# Checking also to see which order polynomial to use
single <- lm(Case_Rate ~ Dose_Rate, data = disease_data)
order_2 <- lm(Case_Rate ~ poly(Dose_Rate, 2), data = disease_data)
order_3 <- lm(Case_Rate ~ poly(Dose_Rate, 3), data = disease_data)
order_4 <- lm(Case_Rate ~ poly(Dose_Rate, 4), data = disease_data)
order_5 <- lm(Case_Rate ~ poly(Dose_Rate, 5), data = disease_data)

anova(single, order_2)
anova(order_2, order_3) # Third is best
anova(order_3, order_4)
anova(order_4, order_5)

