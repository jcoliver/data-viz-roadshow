# Brownian motion proof of concept
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-14

library(ggplot2)
library(dplyr)

num_gen <- 100
num_reps <- 10
step <- 1

out <- NULL
for (rep in 1:num_reps) {
  one_rep <- data.frame(rep = rep,
                        gen = 1:num_gen,
                        pos = NA)
  for (gen in 1:num_gen) {
    if (gen == 1) {
      position <- 0
    } else {
      position <- one_rep$pos[gen - 1] + rnorm(n = 1, 
                                               mean = 0,
                                               sd = step)
    }
    one_rep$pos[gen] <- position
  }
  if (is.null(out)) {
    out <- one_rep
  } else {
    out <- out %>%
      dplyr::bind_rows(one_rep)
  }
}

ggplot(data = out,
       mapping = aes(x = gen, y = pos, 
                     group = rep)) +
  geom_line()
