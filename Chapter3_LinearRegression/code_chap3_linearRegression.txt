library(MASS)
library(ISLR)

attach(Boston)
fit1 = lm(medv~lstat*chas)
fit2 = lm(medv~lstat*chas + I(lstat^2)*chas)

par(mfrow=c(2,1))
plot(medv~lstat,col=ifelse(chas==0,"blue","red"))
points(lstat, fitted(fit1), pch=20, col=ifelse(chas==0,"blue","red"))

plot(medv~lstat,col=ifelse(chas==0,"blue","red"))
points(lstat, fitted(fit2), pch=20, col=ifelse(chas==0,"blue","red"))

fit3 = lm(medv~lstat*as.factor(rad))
fit4 = lm(medv~lstat*as.factor(rad) + I(lstat^2)*as.factor(rad))
plot(medv~lstat)
points(lstat, fitted(fit3), pch=20, col=rad)
