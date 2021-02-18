# Example 1
#
# An optical firm purchases glass to be ground into lenses. As it is
# important that the various pieces of glass have nearly the same
# index of refraction, the firm is interested in controlling the
# variability. A random sample of size n = 20 measurements yields
# s^(2) = 1.2 × 10^(−4) . From previous experience, it is known that the
# normal distribution is a reasonable model for the population of
# these measurements. Find a 95% confidence interval for σ.

# Known variables
n <- 20
degfr <- n - 1
samp_var <- 1.2 * 10^-4
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

# Example 2
#
# In 16 test runs the gasoline consumption of an experimental engine
# had a standard deviation of 2.2 gallons. Construct a 90%
# confidence interval for σ^(2) , which measures the true variability of
# the gasoline consumption of the engine.

# Known variables
n <- 16
degfr <- n - 1
samp_sd <- 2.2
samp_var <- samp_sd^2
CI <- .90
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
n <- 10
x_degfr <- n - 1
x_samp_sd <- 0.5
x_samp_var <- x_samp_sd^2

m <- 8
y_degfr <- m - 1
y_samp_sd <- 0.7
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

#############################################################################

# Example 4
#
# The following data represent the running times of films produced
# by two motion-picture companies.
# Company   Time (minutes)
# I         103 94 110 87 98
# II        97 82 123 92 175 88 118
# Assuming that the two sets of data are independent random
# samples from normal populations, find a 90% confidence interval
# for σ^(2),I /  for σ^(2),II.

# Known variables
n <- 5
x_degfr <- n - 1
x <- c(103, 94, 110, 87, 98)
mean(x)
sd(x)
var(x)

m <- 7
y_degfr <- m - 1
y <- c(97, 82, 123, 92, 175, 88, 118)
mean(y)
sd(y)
var(y)

CI <- .90
alpha <- 1 - CI

# Calculate unknowns
upper_crit <- 1 / qf(alpha/2, x_degfr, y_degfr)
upper_crit
lower_crit <- qf(alpha/2, y_degfr, x_degfr)
lower_crit

lower_bound <- lower_crit * (var(x) / var(y))
upper_bound <- upper_crit * (var(x) / var(y))

# Confidence interval for σ^(2),A / σ^(2),B
c(lower = lower_bound, variance = var(x) / var(y), upper = upper_bound)
