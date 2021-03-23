# 6.5-6. Let x and y equal the ACT scores in social sci-
# ence and natural science, respectively, for a student who
# is applying for admission to a small liberal arts college. A
# sample of n = 15 such students yielded the following data:

x <- c(32, 23, 23, 23, 26, 30, 17, 20, 17, 18, 26, 16, 21, 24, 30)
y <- c(28, 25, 24, 32, 31, 27, 23, 30, 18, 18, 32, 22, 28, 31, 26)

# (a) Calculate the least squares regression line for these
# data.

cor(x,y)

mean(x)

m <- lm(y ~ x)
# m
# 
# abline(m, col = "red")

m1 <- lm(y ~ I(x - mean(x)))
m1

# (b) Plot the points and the least squares regression line on
# the same graph.

plot(x, y, xlab = "Social Science Scores (ACT)",
     ylab = "Natural Science Scores (ACT)", pch=16)

abline(m, col = "red")

# (c) Find point estimates for α, β, and σ^2 .

# summary(m1)
# y_i_hat
resid(m1)
# (y_i - y_i_hat)^2
resid(m1)^2
# point estimator for σ^2
sum(resid(m1)^2)/15