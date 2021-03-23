x <- c(2.0, 3.3, 3.7, 2.0, 2.3, 2.7, 4.0, 3.7, 3.0, 2.3)
y <- c(1.3, 3.3, 3.3, 2.0, 1.7, 3.0, 4.0, 3.0, 2.7, 3.0)

plot(x, y, xlab = "Predicted Grades",
     ylab = "Earned Grades", pch=16)

cor(x,y)

m <- lm(y ~ x)
m

abline(m, col = "red")

m1 <- lm(y ~ I(x - mean(x)))
m1

summary(m1)

# MSE
summary(m1)$sigma^2

mean(x)
mean(y)

# CI for slope parameter beta
confint(m1, level = 0.95)

predict(m, newdata = data.frame(x = 85), interval = "confidence")
predict(m, newdata = data.frame(x = 85), interval = "confidence", level = 0.95)

predict(m, newdata = data.frame(x = 85), interval = "predict")

resid(m1)

plot(x,resid(m1),ylab="Residuals",xlab="x", main="Residual plot for data in Table 6.5-1", pch=16)
abline(h=0) #adds a line through the x-axis

#Difference between confidence and prediction intervals
# Predict these data
predx <- data.frame(x = seq(from = min(x), to = max(x), by = 0.1))

# Confidence interval
conf.int <- cbind(predx, predict(m, newdata = predx, interval = "confidence"))

# Prediction interval
pred.int <- cbind(predx, predict(m, newdata = predx, interval = "prediction"))

# Graph
plot(x, y, ylim = c(65, 105))
abline(m, col = "red")
lines(conf.int[,c(1,3)], col="blue", lty=2, lwd = 2)
lines(conf.int[,c(1,4)], col="blue", lty=2, lwd = 2)
lines(pred.int[,c(1,3)], col="green", lty=3, lwd = 2)
lines(pred.int[,c(1,4)], col="green", lty=3, lwd = 2)
legend("topleft", c("The least squares regression line", "Confidence band",
                    "Prediction band"), col=c("red", "blue", "green"), lty=c(1:3), cex=1.3, lwd=2)
