library(ggplot2)

#####################################
# q2

set.seed(123)
index <- sample(1:nrow(diamonds), 50) # try a subset first
diamonds2 <- diamonds[index,]
diamonds2
hist(diamonds2$price)
hist(diamonds2$price, breaks=quantile(diamonds2$price))


summary(diamonds2$price)
cheap <- diamonds2$price < 4000
cheap
sum(cheap)

######################################
# q1
getwd()
salary <- read.csv("C:/Users/Sophie/Documents/Statistics/hw1/Salaries.csv")
dim(salary)

set.seed(123)
index <- sample(1:nrow(salary), 2000)
salary2 <- salary[index,]

salary2$BasePay <- sapply(salary2$BasePay, as.numeric)
salary2$OvertimePay <- sapply(salary2$OvertimePay, as.numeric)
salary2$OtherPay <- sapply(salary2$OtherPay, as.numeric)
salary2$Benefits <- sapply(salary2$Benefits, as.numeric)
salary2$Year <- sapply(salary2$Year, factor)
salary2$Notes <- sapply(salary2$Notes, as.factor)
sapply(salary2, class)
sapply(salary2, summary)

hist(salary2$BasePay)
hist(salary2$OvertimePay)
hist(salary2$OtherPay)
hist(salary2$Benefits)
hist(salary2$TotalPay)
hist(salary2$TotalPayBenefits)

plot(salary2$Year)
plot(salary2$JobTitle)
plot(salary2$Status)
plot(salary2$Notes)
plot(salary2$Agency)

plot(TotalPay ~ BasePay, data=salary2)
plot(TotalPay ~ Year, data=salary2)


