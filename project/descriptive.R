X <- read.csv("C:/Users/Sophie/workspace/Personality/dataset/survey/BigFivePre.csv", header=TRUE)
X <- X[,c(2:6)]
summary(X$extra)
hist(X$extra)
colMeans(X)
library(moments)
skewness(X$extra)

x <- X$extra
x <- (x-mean(x))/sd(x)
qqnorm(x)
qqline(x)