# Making data for figure 3 (3d bar chart)
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-09-07

library(dplyr)
library(tidyr)
library(palmerpenguins)

penguin_summary <- penguins %>%
  filter(island != "Torgersen") %>%
  filter(!is.na(sex)) %>%
  # group_by(island, sex, year) %>%
  group_by(sex, island) %>%
  summarise(bl = mean(bill_length_mm, na.rm = TRUE),
            bd = mean(bill_depth_mm, na.rm = TRUE)) %>%
  ungroup()
penguin_summary

penguin_long <- penguin_summary %>%
  pivot_longer(cols = c(bl, bd), 
               names_to = "measurement", 
               values_to = "value")
penguin_long

write.csv(x = penguin_long,
          file = "output/figure-3-data.csv",
          row.names = FALSE)
