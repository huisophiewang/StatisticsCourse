X <- read.csv("C:/Users/Sophie/workspace/Personality/dataset/survey/BigFivePre_oncampus.csv", header=TRUE)
hist(X$extra)
boxplot(X$extra, main="extroversion")
summary(X$extra)
library(moments)
skewness(X$extra)
skewness(X$openn)

library(leaps)
X <- read.csv("C:/Users/Sophie/workspace/Personality/result/feature/all_features_extra_test.csv", header=TRUE)
reg1= regsubsets(extra~., data=X, nvmax = 4, method = "exhaustive")
summary(reg1)

X <- read.csv("C:/Users/Sophie/workspace/Personality/result/feature/all_features_all_traits.csv", header=TRUE)
index <- c(2:26)
for(i in index){
  Y <- X[,i]
  m1 <- lm(Y ~ X$extra + X$agrbl + X$consc + X$neuro + X$openn)
  print(colnames(X)[i])
  print(summary(m1))
}
