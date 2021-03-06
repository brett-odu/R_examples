x <- carstopping$Speed
y <- carstopping$StopDist

plot(x, y, xlab = "Speed",
     ylab = "StopDist", pch=16)

cor(x,y)

m <- lm(y ~ x)
m

abline(m, col = "red")

new.x <- x - mean(x)
lm(y ~ new.x)

m1 <- lm(y ~ I(x - mean(x)))
m1

summary(m1)

confint(m1, level = 0.95)

predict(m, newdata = data.frame(x = 40), interval = "confidence")
predict(m, newdata = data.frame(x = 40), interval = "confidence", level = 0.95)

resid(m1)

plot(x,resid(m1),ylab="Residuals",xlab="x", main="Residual plot for data in skincancer.txt", pch=16)
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
