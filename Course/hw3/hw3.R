
#set.seed(123)


#for(i in 1:500)

# x <- replicate(1000, {
#   mm <- runif(10)
#   mean(mm)
# })
# hist(x)


ttest.pval <- function(mean, sd, n){
  sample1 <- rnorm(n, mean, sd)
  sample2 <- rnorm(n, 0, sd)
  result <- t.test(sample1, sample2)
  return(result$p.value)
}
means <- c(0, 0.5, 1, 2, 3)
test <- function (mean){
  pvals <- replicate(1000, ttest.pval(mean, 0.01, 10))
  hist(pvals, main=paste0("mean diff: ", mean, ", sd: ", 0.01), xlab="p-value", xlim=c(0,1))
}

lapply(means, test)
