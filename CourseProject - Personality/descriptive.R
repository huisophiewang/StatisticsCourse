X <- read.csv("C:/Users/Sophie/workspace/Personality/dataset/survey/BigFivePre.csv", header=TRUE)
X <- X[,c(2:6)]

cor(X)

summary(X$extra)
hist(X$extra)
library(moments)
skewness(X$extra)

# qqplot
x <- X$extra
x <- (x-mean(x))/sd(x)
qqnorm(x)
qqline(x)

# log transform, and qqplot
x <- X$extra
x <- log(6-x)
x <- (x-mean(x))/sd(x)
qqnorm(x)
qqline(x)

cor(X$neuro, x)
X$agrbl
log(6-X$agrbl)



lapply(X, mean)
lapply(X, var)
lapply(X, median)
lapply(X, min)
lapply(X, max)
lapply(X, skewness)