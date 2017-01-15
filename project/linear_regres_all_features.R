

X <- read.csv("C:/Users/Sophie/workspace/Personality/result/feature/all_features.csv", header=TRUE)
#cor(X[,2:18], use="complete.obs")
#cor(X[,19:23], use="complete.obs")
#cor(X[,19:26], use="complete.obs")
cor(X[,14:18])




#########################################
# extra
Y <- X[,14]

index <- c(2:13)
for(i in index){
  m1 <- lm(Y ~ X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# add start_time_var (i=3)

index <- c(2,4,5,6,7,8,9,10,11,12,13)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# add len_var (i=2)

index <- c(4,5,6,7,8,9,10,11,12,13)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,2] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add absent (i=7)

index <- c(4,5,6,8,9,10,11,12,13)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,2] + X[,7] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# stop
# final model (i=2,3,7)
m1 <- lm(Y ~ X[,2] + X[,3] + X[,7])
print(summary(m1))
confint(m1, "len_var")

m2 <- lm(Y ~ X[,2] + X[,3] + X[,7] + X[,8])
print(summary(m2))

#########################################
# consc

Y <- X[,16]

index <- c(2:13)
for(i in index){
  m1 <- lm(Y ~ X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add questions (i=12)

index <- c(2,3,4,5,6,7,8,9,10,11,13)
for(i in index){
  m1 <- lm(Y ~ X[,12] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add contributions (i=11)

index <- c(2,3,4,5,6,7,8,9,10,13)
for(i in index){
  m1 <- lm(Y ~ X[,12] + X[,11] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add days (i=9)

index <- c(2,3,4,5,6,7,8,10,13)
for(i in index){
  m1 <- lm(Y ~ X[,12] + X[,11] + X[,9] +  X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# stop, final  model
m1 <- lm(Y ~ X[,12])
print(summary(m1))


index <- c(2,4,5,6,7,8,9,10,11,13)
for(i in index){
  m1 <- lm(Y ~ X[,12] + X[,3] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

m2 <- lm(Y ~ X[,12] + X[,3])
print(summary(m2))



#########################################
# neuro

Y <- X[,17]

index <- c(2:13)
for(i in index){
  m1 <- lm(Y ~ X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add views (i=10)

index <- c(2,3,4,5,6,7,8,9,11,12,13)
for(i in index){
  m1 <- lm(Y ~ X[,10] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# final model 
m1 <- lm(Y ~ X[,10])
print(summary(m1))

###########
# openn
Y <- X[,18]

index <- c(2:13)
for(i in index){
  m1 <- lm(Y ~ X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# add answers (i=13)

index <- c(2,3,4,5,6,7,8,9,10,11,12)
for(i in index){
  m1 <- lm(Y ~ X[,13] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}
# final model
m1 <- lm(Y ~ X[,13])
print(summary(m1))
