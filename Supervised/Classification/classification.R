#sample(x, size, replace = False, prob = NULL)
#probability = c("Bronze", "Bronze", "Bronze", "Black", "Red", "Red", "Brown", "Brown", "Brown", "Brown")
#samples = sample(probability, 100, replace=TRUE)
#samples
L <- sapply(list("Bronze", "Black", "Red", "Brown"), length)
samples = sample(c("Bronze", "Black", "Red", "Brown"),
                 size = 100,
                 prob = rep(c(0.3, 0.1, 0.2, 0.4) / L, L),
                 replace = TRUE)
