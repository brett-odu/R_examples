# 6.5-4. The final grade in a calculus course was predicted
# on the basis of the student’s high school grade point aver-
# age in mathematics, Scholastic Aptitude Test (SAT) score
# in mathematics, and score on a mathematics entrance
# examination. The predicted grades x and the earned
# grades y for 10 students are given (2.0 represents a C, 2.3
# a C+, 2.7 a B–, etc.).

x <- c(2.0, 3.3, 3.7, 2.0, 2.3, 2.7, 4.0, 3.7, 3.0, 2.3)
y <- c(1.3, 3.3, 3.3, 2.0, 1.7, 3.0, 4.0, 3.0, 2.7, 3.0)

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

plot(x, y, xlab = "Predicted Grades",
     ylab = "Earned Grades", pch=16)

abline(m, col = "red")

# (c) Find the value of σ^2

# y_i_hat
resid(m1)
# (y_i - y_i_hat)^2
e = resid(m1)^2
sum(e)

# point estimator for σ^2_hat
sum(e)/length(x)

# 7.6-4. For the data given in Exercise 6.5-4, with the usual
# assumptions,

summary(m1)

# MSE
summary(m1)$sigma^2
sum(e)/(length(x)-2)

# (a) Find a 95% confidence interval for μ(x) when x = 2, 3,
# and 4.

predict(m, newdata = data.frame(x = 2), interval = "confidence", level = 0.95)
predict(m, newdata = data.frame(x = 3), interval = "confidence", level = 0.95)
predict(m, newdata = data.frame(x = 4), interval = "confidence", level = 0.95)

# (b) Find a 95% prediction interval for Y when x = 2, 3,
# and 4.

predict(m, newdata = data.frame(x = 2), interval = "predict")
predict(m, newdata = data.frame(x = 3), interval = "predict")
predict(m, newdata = data.frame(x = 4), interval = "predict")


# 7.6-16. Find 95% confidence intervals for α, β, and σ^2 for
# the predicted and earned grades data in Exercise 6.5-4.

# CI for regression parameters α and β.
confint(m1, level = 0.95)

# CI for regression parameter σ^2.
# ????????????????