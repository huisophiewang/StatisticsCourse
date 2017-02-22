X <- read.csv("C:/Users/Sophie/workspace/Personality/result/feature/all_features_neuro.csv", header=TRUE)
hist(X$neuro)
boxplot(X$neuro, main="neuroticism")

Y <- X[,ncol(X)]
index <- c(2:3)
for(i in index){
  m1 <- lm(Y ~ X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add end_time_var

index <- c(3)
for(i in index){
  m1 <- lm(Y ~ X[,2] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add bluetooth_daytime