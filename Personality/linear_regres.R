library(ggplot2)

X <- read.csv("C:/Users/Sophie/Documents/Statistics/project/feature/all_wifi_features.csv", header=TRUE)

# extroversion X[,16]
# activity X[,22]
Y <- X[,16]
#########################################
# Exploratory data analysis
#########################################

head(X)
hist(X$extra)
boxplot(X$extra, main="extroversion")
cor(X[,2:8])
pairs(X[,2:8])

#########################################
# forward selection
#########################################

# step 1, add X[3] len_diff
index <- c(2:15)
for(i in index){
  m1 <- lm(Y ~ X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# step 2, add X[8] end_time_var
index <- c(2,4,5,6,7,8,9,10,11,12,13,14,15)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# step 3, add X[9] fp_home
index <- c(2,4,5,6,7,9,10,11,12,13,14,15)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,8] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# step 4
index <- c(2,4,5,6,7,10,11,12,13,14,15)
for(i in index){
  m1 <- lm(Y ~ X[,3] + X[,8] + X[,9] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}

# candidate 1
m1 <- lm(Y ~ X[,3] + X[,8] + X[,9])
summary(m1)

# candidate 2 (best model)
m2 <- lm(Y ~ X[,4] + X[,8] + X[,9])
summary(m2)
anova(m2)
plot(predict(m2), X$extra)

#########################################
# model diagnostics
#########################################
plot(X[,4], X$extra, xlab="len_var", ylab="extroversion")
plot(X[,8], X$extra, xlab="end_time_var", ylab="extroversion")
plot(X[,9], X$extra, xlab="freq_home", ylab="extroversion")

X <- na.omit(X)
m2 <- lm(extra ~ X[,4] + X[,8] + X[,9], data=X)
residual <- resid(m2)

# residual against Y
plot(X$extra, residual, xlab="extroversion")

# Q-Q plot
qqnorm(residual)
qqline(residual)

# residual against X
plot(X[,4], residual, xlab="len_var")
plot(X[,8], residual, xlab="end_time_var")
plot(X[,9], residual, xlab="freq_home")

#########################################
# parameter estimation
#########################################

# 95% CI
# t(0.975, 20)=2.086


#########################################
# lack of fit test
#########################################


#########################################
# use activity as response variable

Y <- X[,22]

index <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
for(i in index){
  m1 <- lm(Y ~ X[,2] + X[,8] + X[,13] + X[,i])
  print(colnames(X)[i])
  print(summary(m1))
}



