# City heat data prep
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-07-05

library(dplyr)
library(ggplot2)

################################################################################
# Reduction from large dataset to three Arizona cities

# Data from https://kilthub.cmu.edu/articles/dataset/Compiled_daily_temperature_and_precipitation_data_for_the_U_S_cities/7890488/5

# Large data file (>500 MB), not under version control
# all_cities <- read.csv(file = "~/Desktop/city-heat/all_cities.csv")

# Arizona cities of interest; first pull out IDs from the ID file
# city_info <- read.csv(file = "~/Desktop/city-heat/city_info.csv")
# arizona_ids <- city_info[grep(pattern = "Tucson|Phoenix|Flagstaff",
#                               x = city_info$Name), ]
# Only need city names and ID
# arizona_ids <- arizona_ids[, c("Name", "ID")]
# arizona_ids <- unique(arizona_ids)
# colnames(arizona_ids)[2] <- "id"

# Now extract rows from the large data file
# arizona_heat <- all_cities[all_cities$id %in% arizona_ids$id, ]
# Merge in city name for easier data wrangling
# arizona_heat <- merge(x = arizona_heat, 
                      # y = arizona_ids, 
                      # by = "id")

# Switch over to using tidyverse, because we are lazy?
# Select only those necessary columns
# arizona_heat <- arizona_heat %>%
#   select(id, Date, tmax, tmin, prcp, Name) %>%
#   rename(date = Date,
#          name = Name)

# Go ahead and write this file to disk
# write.csv(file = "data/arizona-heat.csv",
#           x = arizona_heat,
#           row.names = FALSE)

################################################################################
# Boxplot & summary stats of average start of summer high temperatures
arizona_heat <- read.csv(file = "data/arizona-heat.csv")

summer_solstice <- arizona_heat[grep(pattern = "[0-9]{4}-06-21",
                                     x = arizona_heat$date), ]

summer_summary <- summer_solstice %>%
  group_by(name) %>%
  summarize(mean_tmax = mean(tmax, na.rm = TRUE),
            sd_tmax = sd(tmax, na.rm = TRUE))
summer_summary

summer_box <- ggplot(data = summer_solstice, mapping = aes(x = name, y = tmax)) +
  geom_boxplot() +
  theme_bw() +
  xlab(label = "City") +
  ylab(label = expression(High~temperature~"("*degree*F*")")) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))

print(summer_box)
ggsave(file = "output/boxplot-heat.png", plot = summer_box)

################################################################################
# Scatterplot of summer solstice temperatures by year for Tucson
# tucson_summer <- summer_solstice %>%
#   filter(name == "Tucson") %>%
#   mutate(year = substr(x = date, start = 1, stop = 4)) %>%
#   mutate(year = as.integer(year)) %>%
#   na.omit()

# write.csv(file = "data/tucson-summer.csv",
#           x = tucson_summer,
#           row.names = FALSE)

tucson_summer <- read.csv(file = "data/tucson-summer.csv")

# Pull out a few rows for hand-plot exercise
decades <- seq(from = 1921, to = 2021, by = 20)
hand_plot <- tucson_summer %>%
  filter(year %in% decades) %>%
  select(year, tmax)
hand_plot

# Plot with linear regression
ggplot(data = tucson_summer, mapping = aes(x = year, y = tmax)) +
  geom_point() +
  geom_smooth(method = "lm")
simple_lm <- lm(tmax ~ year, data = tucson_summer)

# Plot with second-order polynomial
ggplot(data = tucson_summer, mapping = aes(x = year, y = tmax)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2))
poly_2 <- lm(tmax ~ poly(x = year, degree = 2), data = tucson_summer)
anova(simple_lm, poly_2)

# Plot with third-order polynomial
ggplot(data = tucson_summer, mapping = aes(x = year, y = tmax)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3))
poly_3 <- lm(tmax ~ poly(x = year, degree = 3), data = tucson_summer)
anova(simple_lm, poly_3)


