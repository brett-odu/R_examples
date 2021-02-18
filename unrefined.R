install.packages("maxLik")

search()
library(maxLik)

set.seed(123)
x <- rnorm(100, mean = 1, sd = 2)
logLikFun <- function(param) {
  mu <- param[1]
  sigma <- param[2]
  sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}
mle <- maxLik(logLik = logLikFun, start = c(mu = 0, sigma = 1))
summary(mle)

x <- c(55.95, 56.54, 57.58, 55.13, 57.48, 56.06, 59.93, 58.30, 52.57, 58.46)
mean(x)
mean.x
mu <- mean(x)
mu
mu(x)
mu.x
sigma.x
sigma(x)
?qnorm
t.test(x)
norm.interval = function(data, variance = var(data), conf.level = 0.95) {
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  c(xbar - z * sdx, xbar + z * sdx)
}
norm.interval(x)
norm.interval(x, 4)
prob(x<52)
pnorm(x, mu, 2)
pnorm(52, mu, 2)

ggplot(dnorm(x)) +
  geom_density()

x <- rnorm(1000, mean=100, sd=15)
hist(x, probability=TRUE)
xx <- seq(min(x), max(x), length=100)
lines(xx, dnorm(xx, mean=100, sd=15))
  