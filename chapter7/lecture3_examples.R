##################################################################################################
# Example 1
cats <- c(20,21,35,13,21,10)
dogs <- c(31,10,20,40)

# sample mean
mean.cats <- (20 + 21 + 35 + 13 + 21 + 10)/6
mean.cats
mean(cats)

mean.dogs <- (31 + 10 + 20 + 40)/4
mean.dogs
mean(dogs)

# sample variance
s2.cats <- ((20 - 20)^2 + (21 - 20)^2 + (35 - 20)^2 + (13 - 20)^2 + (21 - 20)^2 + (10 - 20)^2)/(6-1)
s2.cats
var(cats)

s2.dogs <- ((31 - 25.25)^2 + (10 - 25.25)^2 + (20 - 25.25)^2 + (40 - 25.25)^2)/(4-1)
s2.dogs
var(dogs)

# sample size
n <- length(cats)
m <- length(dogs)

# Pooled sample variance
sp2 <- ((n-1)*var(cats) +(m-1)*var(dogs))/(n+m-2)
sp2

# Critical value
alpha <- 0.10
crit <- qt(alpha/2, df = n+m-2, lower.tail = F)
crit

# CI
(mean(cats)-mean(dogs)) + c(-1,1) * crit * sqrt(sp2*(1/n+1/m))

# Using t.test
t.test(cats, dogs, alternative = "two.sided", var.equal = TRUE, conf.level = 1-alpha)

ggplot(data = cats_and_dogs) + geom_boxplot(aes(x = group, y = weight)) + labs(title = "Animal Weights")

##################################################################################################
# Example 2 (a)
x <- c(12.9, 10.2, 7.4, 7.0, 10.5, 11.9, 7.1, 9.9, 14.4, 11.3)
y <- c(10.2, 6.9, 10.9, 11.0, 10.1, 5.3, 7.5, 10.3, 9.2, 8.8)

# sample size
n <- length(x)
m <- length(y)

# Pooled sample variance
sp2 <- ((n-1)*var(x) +(m-1)*var(y))/(n+m-2)
sp2

# Critical value
alpha <- 0.05
crit <- qt(alpha/2, df = n+m-2, lower.tail = F)
crit

# CI
(mean(x)-mean(y)) + c(-1,1) * crit * sqrt(sp2*(1/n+1/m))

# Using t.test
t.test(x, y, alternative = "two.sided", var.equal = TRUE, conf.level = 1-alpha)

##################################################################################################
# Example 2 (b)
x <- c(12.9, 10.2, 7.4, 7.0, 10.5, 11.9, 7.1, 9.9, 14.4, 11.3)
y <- c(10.2, 6.9, 10.9, 11.0, 10.1, 5.3, 7.5, 10.3, 9.2, 8.8)

# sample size
n <- length(x)
m <- length(y)

# degrees of freedom
r <- floor((var(x)/n + var(y)/m)^2/((var(x)/n)^2/(n-1) + (var(y)/m)^2/(m-1)))
r


# Critical value
alpha <- 0.05
crit <- qt(alpha/2, df = r, lower.tail = F)
crit

# CI
(mean(x)-mean(y)) + c(-1,1) * crit * sqrt(var(x)/n+ var(y)/m)


# Using t.test
t.test(x, y, alternative = "two.sided", var.equal = FALSE, conf.level = 1-alpha)


##################################################################################################
# Example 3
x <- c(1.94, 1.44, 1.56, 1.58, 2.06, 1.66, 1.75, 1.77, 1.78, 1.92, 1.25, 1.93, 2.04, 1.62, 2.08)
y <- c(1.27, 1.63, 1.47, 1.39, 1.93, 1.26, 1.71, 1.67, 1.28, 1.85, 1.02, 1.34, 2.02, 1.59, 1.97)
d <- x - y
cbind(x, y, d)

# sample mean
mean(d)

# sample variance/sd
var(d)
sqrt(var(d))
sd(d)

# sample size
n <- length(d)

# Critical value
alpha <- 0.05
crit <- qt(alpha/2, df = n-1, lower.tail = F)
crit

# CI
mean(d) + c(-1,1) * crit * sd(d)/sqrt(n)

# Using t.test
t.test(x, y, alternative = "two.sided", paired = TRUE, conf.level = 1-alpha)





