# Data preparation for Megastore Jupyter notebooks
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-11-10

library(ggplot2)
library(dplyr)

sales <- read.csv(file = "data/Megastore_Sales_Data.csv")

# Data to plot for Notebook
annual_sales <- sales %>%
  filter(!(Outlet_Year %in% c(2005, 2007))) %>% # Too few obs in 2005 & 2007
  filter(Item_Type == "Fruits and Vegetables") %>%
  filter(Outlet_Location_Type == "Tier 3") %>%
  group_by(Outlet_Year) %>%
  summarize(Total_Sales = sum(Sales, na.rm = TRUE),
            Mean_MRP = mean(Item_MRP, na.rm = TRUE)) %>%
  rename(Year = Outlet_Year) %>%
  ungroup()

write.csv(file = "data/Megastore_Produce.csv",
          x = annual_sales,
          row.names = FALSE)

ggplot(data = annual_sales, mapping = aes(x = Year, 
                                          y = Total_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))

# Do model testing
lm_1 <- lm(Total_Sales ~ Year, data = annual_sales)
lm_2 <- lm(Total_Sales ~ poly(x = Year, degree = 2), data = annual_sales)
anova(lm_1, lm_2)
lm_3 <- lm(Total_Sales ~ poly(x = Year, degree = 3), data = annual_sales)
anova(lm_1, lm_3)
