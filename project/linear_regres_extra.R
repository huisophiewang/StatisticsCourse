#X <- read.csv("C:/Users/Sophie/workspace/Personality/result/feature/all_features.csv", header=TRUE)
X <- read.csv("C:/Users/Sophie/workspace/Personality/result/feature/combined_all_extra.csv", header=TRUE)
#########################################
# extra
Y <- X[,ncol(X)]

index <- c(1:14)
for(i in index){
  m1 <- lm(Y ~ X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add start_time_var (i=3)

index <- c(1,2,4,5,6,7,8,9,10,11,12,13,14)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add len_var (i=2)

index <- c(1,4,5,6,7,8,9,10,11,12,13,14)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,2] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add absent (i=7)

index <- c(1,4,5,6,8,9,10,11,12,13,14)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,2] + X[,7] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# final model (i=2,3,7)
m1 <- lm(Y ~ X[,2] + X[,3] + X[,7])
print(summary(m1))

m2 <- lm(Y ~ X[,2] + X[,3] + X[,7] + X[,11])
print(summary(m2))