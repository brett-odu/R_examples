# Example 1
#
# When the 2000 General Social Survey asked subjects if they would
# be willing to accept cuts in their standard of living to protect the
# environment, 344 of 1170 subjects said yes.
# (a) Estimate the population proportion, p, who would answer yes.
# (b) Calculate a 90% CI for p.
# (c) Now calculate a 98% CI for p.
# (d) Similarly, a 99% CI for p can be shown to equal (0.261, 0.330). 
#     What pattern do you see?

# Known variables
n <- 1170
y <- 344
CI <- 0.90
alpha <- 1 - CI

# Calculate unknowns
p_hat <- y/n
p_hat
SE <- sqrt(p_hat * (1 - p_hat) / n)
crit <-  qnorm(alpha/2, lower.tail = FALSE)
crit

# Function to calculate Z-Interval
z.interval <- function(p_hat, crit, SE) {
  c(lower = p_hat - crit * SE, upper = p_hat + crit * SE)
}

# Calculation of Z-Interval
z.interval(p_hat, crit, SE)

# Change confidence interval
CI <- 0.98
alpha <- 1 - CI

# Calculate unknowns
p_hat <- y/n
p_hat
SE <- sqrt(p_hat * (1 - p_hat) / n)
crit <-  qnorm(alpha/2, lower.tail = FALSE)
crit
z.interval(p_hat, crit, SE)

#############################################################################

# Example 2
#
# Based on the sample of 10 coin flips of which 10 turn up heads,
# calculate a 95% small sample confidence interval for p, the
# probability of flipping a head in a toss of a fair coin (assuming we
# don’t know p = 0.5).

# Known variables
n <- 10
y <- 10
CI <- 0.95
alpha <- 1 - CI

# Calculate unknowns
p_tilde <- (y + 2) / (n + 4)
p_tilde
SE <- sqrt(p_tilde * (1 - p_tilde) / (n + 4))
crit <-  qnorm(alpha/2, lower.tail = FALSE)
crit

# Function to calculate Z-Interval
z.interval <- function(p_tilde, crit, SE) {
  c(lower = p_tilde - crit * SE, upper = p_tilde + crit * SE)
}

# Calculation of Z-Interval
z.interval(p_tilde, crit, SE)

#############################################################################

# Example 3
#
# The NCAA requires colleges to report the graduation rates of their
# athletes. In a sample of former male and female student athletes,
# 43 of the 53 females surveyed had graduated whereas 58 of the
# 102 males had graduated. Calculate and interpret a 99%
# confidence interval for the difference in true proportions of female
# athletes who graduate p(F) and male athletes who graduate p(M).

# Known variables
n1 <- 53
y1 <- 43
n2 <- 102
y2 <- 58

CI <- 0.99
alpha <- 1 - CI

# Calculate unknowns
p_hat_F <- y1 / n1
p_hat_F
p_hat_M <- y2 / n2
p_hat_M
SE <- sqrt((p_hat_F * (1 - p_hat_F) / n1) + 
           (p_hat_M * (1 - p_hat_M) / n2))
crit <-  qnorm(alpha/2, lower.tail = FALSE)
crit

# Function to calculate Z-Interval
z.interval2pop <- function(p_hat_F, p_hat_M, crit, SE) {
  c(lower = (p_hat_F - p_hat_M) - crit * SE, 
    upper = (p_hat_F - p_hat_M) + crit * SE)
}

# Calculation of Z-Interval
z.interval2pop(p_hat_F, p_hat_M, crit, SE)

#############################################################################

# Example 4
#
# SurveyUSA polled 500 Americans and asked if marijuana should be
# legalized for medicinal purposes. The results of this survey are
# summarized in the following contingency table:

#                   Legalize        Don’t legalize       Total
# < 50 years old       202               108              310
# ≥ 50 years old       118                72              190
#          Total       320               180              500
#
# Calculate and interpret a 95% confidence interval for the difference
# in true proportions of younger people ( < 50 years old) who think
# marijuana should be legalized for medicinal purposes and older
# people (≥ 50 years old) who think marijuana should be legalized
# for medicinal purposes.

# Known variables
n1 <- 310
y1 <- 202
n2 <- 190
y2 <- 118

CI <- 0.95
alpha <- 1 - CI

# Calculate unknowns
p_hat_1 <- y1 / n1
p_hat_1
p_hat_2 <- y2 / n2
p_hat_2
SE <- sqrt((p_hat_1 * (1 - p_hat_1) / n1) + 
             (p_hat_2 * (1 - p_hat_2) / n2))
crit <-  qnorm(alpha/2, lower.tail = FALSE)
crit

# Function to calculate Z-Interval
z.interval2pop <- function(p_hat_1, p_hat_2, crit, SE) {
  c(lower = (p_hat_1 - p_hat_2) - crit * SE, 
    upper = (p_hat_1 - p_hat_2) + crit * SE)
}

# Calculation of Z-Interval
z.interval2pop(p_hat_1, p_hat_2, crit, SE)
