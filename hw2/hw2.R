library(ggplot2)
require(graphics)

summary(diamonds$price)
plot(diamonds$price)
hist(diamonds$price)

boxplot(diamonds$price)
boxplot(log(diamonds$price))

x <- diamonds$price
x <- (x-mean(x))/sd(x)
qqnorm(x)
qqline(x)

log_x <- log(diamonds$price)
log_x <- (log_x-mean(log_x))/sd(log_x)
qqnorm(log_x)
qqline(log_x)


y <- rt(200, df = 5)
qqnorm(y); qqline(y, col = 2)
qqplot(y, rt(300, df = 5))


sample_mean <- function(x, sz, n) {
  res <- vector()
  smp <- sample(x, size=sz)
  res[n] <- mean(smp)
}

sample_mean <- function(data, size, n){
  means <- vector(mode='numeric',length=n)
  for(i in 1:n){
    sample1 <- sample(data, size)
    means[i] <- mean(sample1)
  }
  hist(means)
}

sample_mean(diamonds$price, 3, 100)



ranges <- list()
for ( name in names(diamonds) )
  if ( is.numeric(diamonds[[name]]) )
    ranges[[name]] <- range(diamonds[[name]])
ranges[1:4]
ranges

pnorm(66, mean=65, sd=0.25, lower.tail=FALSE)
