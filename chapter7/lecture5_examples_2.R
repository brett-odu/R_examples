# 1) 7.3-2
#
# Let p equal the proportion of letters mailed in the
# Netherlands that are delivered the next day. Suppose that
# y = 142 out of a random sample of n = 200 letters were
# delivered the day after they were mailed.
# (a) Give a point estimate of p.
# (b) Use Equation 7.3-2 to find an approximate 90% con-
#   fidence interval for p.
# (c) Use Equation 7.3-4 to find an approximate 90% con-
#   fidence interval for p.
# (d) Use Equation 7.3-5 to find an approximate 90% con-
#   fidence interval for p.
# (e) Find a one-sided 90% confidence interval for p that
# provides a lower bound for p.

# (a)
# Known variables
n <- 200
y <- 142
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

# (b)


# (c)
# Known variables
n <- 200
y <- 142
CI <- 0.90
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

# 2) 7.3-9
#
# Consider the following two groups of women:
# Group 1 consists of women who spend less than $500
# annually on clothes; Group 2 comprises women who
# spend over $1000 annually on clothes. Let p 1 and p 2 equal
# the proportions of women in these two groups, respectively, 
# who believe that clothes are too expensive. If 1009
# out of a random sample of 1230 women from group 1 and
# 207 out of a random sample 340 from group 2 believe that
# clothes are too expensive,
# (a) Give a point estimate of p(1) − p(2).
# (b) Find an approximate 95% confidence interval for
# p(1) − p(2).

# Known variables
n1 <- 1230
y1 <- 1009
n2 <- 340
y2 <- 207

CI <- 0.95
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

# Example 2
#
# In 16 test runs the gasoline consumption of an experimental engine
# had a standard deviation of 2.2 gallons. Construct a 90%
# confidence interval for σ^(2) , which measures the true variability of
# the gasoline consumption of the engine.

# Known variables
n <- 10
degfr <- n - 1
samp_sd <- 0.29
samp_var <- samp_sd^2
CI <- .95
alpha <- 1 - CI

# Calculate unknowns
a <- qchisq(1 - alpha/2, degfr, lower.tail = FALSE)
a
b <- qchisq(alpha/2, degfr, lower.tail = FALSE)
b
lower <- degfr * samp_var / b
upper <- degfr * samp_var / a

# Create density curve
curve(dchisq(x, degfr), from = 0, to = b*1.25,
      main = "Chi-Square Distribution",
      ylab = 'Density',
      lwd = 2)

# Create vector of x values
x_lower <- seq(0, a)

# Create vector of chi-square density values
p_lower <- dchisq(x_lower, degfr)

# Fill in portion of the density plot from 0 to lower 95% value
polygon(c(x_lower, rev(x_lower)), c(p_lower, rep(0, length(p_lower))),
        col = adjustcolor('red', alpha=0.3), border = NA)

# Create vector of x values
x_upper <- seq(b, b*1.25)

# Create vector of chi-square density values
p_upper <- dchisq(x_upper, degfr)

# Fill in portion of the density plot for upper 95% value to end of plot
polygon(c(x_upper, rev(x_upper)), c(p_upper, rep(0, length(p_upper))),
        col = adjustcolor('red', alpha=0.3), border = NA)



# Confidence interval for σ^(2)
c(lower = lower, variance = samp_var, upper = upper)

# Confidence interval for σ
c(lower = sqrt(lower), sd = sqrt(samp_var), upper = sqrt(upper))

#############################################################################

# Example 3
#
# A study has been made to compare the nicotine contents of two
# brands of cigarettes. Ten cigarettes of Brand A had an average
# nicotine content of 3.1 milligrams with a standard deviation of 0.5
# milligram, while eight cigarettes of Brand B had an average
# nicotine content of 2.7 milligrams with a standard deviation of 0.7
# milligram. Assuming that the two sets of data are independent
# random samples from normal populations, find a 98% confidence
# interval for σ^(2),A / σ^(2),B.

# Known variables
n <- 61
x_degfr <- n - 1
x_samp_sd <- 19.4
x_samp_var <- x_samp_sd^2

m <- 61
y_degfr <- m - 1
y_samp_sd <- 18.8
y_samp_var <- y_samp_sd^2

CI <- .98
alpha <- 1 - CI

# Calculate unknowns
upper_crit <- 1 / qf(alpha/2, x_degfr, y_degfr)
upper_crit
lower_crit <- qf(alpha/2, y_degfr, x_degfr)
lower_crit

lower_bound <- lower_crit * (x_samp_var / y_samp_var)
upper_bound <- upper_crit * (x_samp_var / y_samp_var)

# Confidence interval for σ^(2),A / σ^(2),B
c(lower = lower_bound, variance = x_samp_var / y_samp_var, upper = upper_bound)