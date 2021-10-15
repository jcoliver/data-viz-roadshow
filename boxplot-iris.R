# Boxplot on iris
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-10-15

library(ggplot2)
library(dplyr)

iris_summary <- iris %>%
  group_by(Species) %>%
  summarize(pl_mean = mean(Petal.Length),
            pl_se = sd(Petal.Length)/sqrt(n()))
iris_summary

iris_box <- ggplot(data = iris,
                   mapping = aes(x = Species, y = Petal.Length)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("setosa" = "I. setosa",
                              "versicolor" = "I. versicolor",
                              "virginica" = "I. virginica")) +
  theme_bw() +
  ylab(label = "Petal Length (mm)") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))

print(iris_box)
ggsave(filename = "output/boxplot-iris.png", plot = iris_box)
