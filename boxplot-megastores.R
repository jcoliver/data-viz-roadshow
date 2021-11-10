# Boxplot on sales data
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-11-10

library(ggplot2)
library(dplyr)
library(stringr)

sales <- read.csv(file = "data/Megastore_Sales_Data.csv")

# Filter to only 2009 data
# We also want things to show up in a certain order (Small, Medium, Large), so 
# re-level the Outlet_Size column
sales_2009 <- sales %>%
  filter(Outlet_Year == 2009) %>%
  mutate(Outlet_Size = str_replace(string = Outlet_Size,
                                   pattern = "High",
                                   replacement = "Large")) %>%
  mutate(Outlet_Size = factor(x = Outlet_Size,
                              levels = c("Small", "Medium", "Large")))

# Do summary stats for table
sales_summary <- sales_2009 %>%
  group_by(Outlet_Size) %>%
  summarize(mrp_mean = mean(Item_MRP, na.rm = TRUE),
            mrp_se = sd(Item_MRP, na.rm = TRUE)/sqrt(n()))
sales_summary

# Boxplot of Maximum Retail Price (MRP) by outlet size
mrp_box <- ggplot(data = sales_2009, mapping = aes(x = Outlet_Size, 
                                                   y = Item_MRP)) +
  geom_boxplot() +
  theme_bw() +
  ylab(label = "Max. Retail Price ($)") +
  xlab(label = "Outlet Size") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))

print(mrp_box)
ggsave(filename = "output/boxplot-megastore.png", plot = mrp_box)