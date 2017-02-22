X <- read.csv("C:/Users/Sophie/workspace/Personality/result/feature/all_features_consc.csv", header=TRUE)
hist(X$consc)
boxplot(X$consc, main="conscientiousness")

Y <- X[,ncol(X)]
index <- c(2:4)
for(i in index){
  m1 <- lm(Y ~ X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add start_time_var

index <- c(3,4)
for(i in index){
  m1 <- lm(Y ~ X[,2] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add piazza_questions

index <- c(3)
for(i in index){
  m1 <- lm(Y ~ X[,2] + X[,4] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}