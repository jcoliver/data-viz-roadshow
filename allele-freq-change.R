# Allele frequency change over time with selection
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-14

# Proof of concept for a notebook

# Calculates allele frequency for next generation
do_selection <- function(p, s) {
  q <- 1 - p
  W11 <- 1
  W_2 <- 1 - s
  W_mean <- p * p * W11 + 2 * p * q * W_2 + q * q * W_2
  p_prime <- ( p * (p * W11 + q * W_2) ) / W_mean
  return(p_prime)
}
# do_selection(p = 0.5, s = 0.01)

num_gen <- 10
pop_size <- 1000
selection <- 0.2

out <- data.frame(gen = 1:num_gen,
                  p_freq = NA)
# Run process, where we use post-selection frequency of alleles 
# to use in sampling of alleles for generation t + 1
for (gen in 1:num_gen) {
  if (gen == 1) {
    p <- 0.1 # start with relatively high frequency
  } else {
    # Calculate p prime from generation t - 1
    p_select <- do_selection(p = out$p_freq[gen - 1], 
                             s = selection)
    # Using p prime, generate sample of alleles for generation t
    pop_sample <- sample(x = c(0, 1),
                         size = pop_size, 
                         prob = c(p_select, (1 - p_select)),
                         replace = TRUE)
    # Find allele frequency for generation t
    p <- sum(pop_sample == 0)/pop_size
  }
  out$p_freq[gen] <- p
}
