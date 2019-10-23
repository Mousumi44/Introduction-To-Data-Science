#5
#(a)
library(ISLR)
attach(Default)
set.seed(1)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

#(b)
#(i)
train <- sample(dim(Default)[1],dim(Default)[1]/2)
#(ii)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
summary(fit.glm)
#(iii)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "yes"
#(iv)
mean(pred.glm != Default[-train, ]$default)

#(c)
train <- sample(dim(Default)[1],dim(Default)[1]/2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "yes"
mean(pred.glm != Default[-train, ]$default)

train <- sample(dim(Default)[1],dim(Default)[1]/2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "yes"
mean(pred.glm != Default[-train, ]$default)

train <- sample(dim(Default)[1],dim(Default)[1]/2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "yes"
mean(pred.glm != Default[-train, ]$default)

#(d)
train <- sample(dim(Default)[1],dim(Default)[1]/2)
fit.glm <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "yes"
mean(pred.glm != Default[-train, ]$default)

#(6)
#(a)
library(ISLR)
attach(Default)
set.seed(1)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

#(b)
boot.fn <- function(data, index){
  fit <- glm(default ~ income + balance, data = Default, family = "binomial", subset = index)
  return (coef(fit))
}

#(c)
library(boot)
boot(Default, boot.fn, 1000)

#(8)
#(a)
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x ^ 2 +rnorm(100)

#(b)
plot(x,y)

#(c)

library(boot)
Data <- data.frame(x, y)

set.seed(1)
#(i)
glm1 <- glm(y ~ x)
cv.glm(Data, glm1)$delta[1]

#(ii)
glm2 <- glm(y ~ poly(x, 2))
cv.glm(Data, glm2)$delta[1]

#(iii)
glm3 <- glm(y ~ poly(x, 3))
cv.glm(Data, glm3)$delta[1]

#(iv)
glm4 <- glm(y ~ poly(x, 4))
cv.glm(Data, glm4)$delta[1]

#(d)
set.seed(10)
#(i)
glm1 <- glm(y ~ x)
cv.glm(Data, glm1)$delta[1]

#(ii)
glm2 <- glm(y ~ poly(x, 2))
cv.glm(Data, glm2)$delta[1]

#(iii)
glm3 <- glm(y ~ poly(x, 3))
cv.glm(Data, glm3)$delta[1]

#(iv)
glm4 <- glm(y ~ poly(x, 4))
cv.glm(Data, glm4)$delta[1]

#(f)
summary(glm4)

#(9)
#(a)
library(MASS)
attach(Boston)
mu.hat <- mean(medv)
mu.hat

#(b)
se.hat <- sd(medv) / sqrt(dim(Boston)[1])
se.hat

#(c)
set.seed(1)
boot.fn <- function(data, index){
  mu <- mean(data[index])
  return (mu)
}
boot(medv, boot.fn, 1000)

#(d)
t.test(medv)
CI.mu.hat <- c(mu.hat - 2*se.hat, mu.hat + 2*se.hat)
CI.mu.hat

#(e)
med.hat <- median(medv)
med.hat

#(f)
boot.fn <- function(data, index){
  md <- median(data[index])
  return (md)
}
boot(medv, boot.fn, 1000)

#(g)
ten.percent.hat <- quantile(medv, c(0.1))
ten.percent.hat

#(h)
boot.fn <- function(data, index){
  tph <- quantile(data[index], c(0.1))
  return (tph)
}
boot(medv, boot.fn, 1000)
