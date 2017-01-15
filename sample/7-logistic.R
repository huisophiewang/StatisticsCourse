######################################################################
#######################################################################
#                 L O G I S T I C   R E G R E S S I O N
#######################################################################
#######################################################################


#######################################################################
#                       Example of Bernouilli distribution 
#                         (data recordered per subect)
#######################################################################
# setwd("/Users/ovitek/Dropbox/Olga/Teaching/CS7280/Spring16/LectureNotes/7-logistic")
X <- read.table("C:/Users/Sophie/Documents/Statistics/sample/smokingAndObesity.txt", sep=" ", as.is=TRUE, header=TRUE)
X <- X[order(X$age),]

# factor for 'smoking status'
X$smokeF <- factor(X$smoke)
head(X)


# create a proper binary response for 'overweight'
table(X$over_wt)
X$over_wtF <- factor(abs(X$over_wt - 2), levels=c(0,1))
table(X$over_wtF)
head(X)

# equivalence of odds ratios in a table and 
# in logistic regression
table(X$smokeF, X$over_wtF)
fit<- glm(over_wtF ~ smokeF, family=binomial, data=X) 
summary(fit)

# odds
exp(-2.81223) # = 85/1415 = 0.06007067
exp(-2.81223-0.06799) # = (22/392) = 0.05612245  
exp(-2.81223-0.17656) # = (94/1867) = 0.05034815

# odds ratio
exp(-0.06799) # = (22/392) / (85/1415) = 0.9342698


# add age
fit<- glm(over_wtF ~ age + smokeF + age*smokeF, family=binomial, data=X) 
summary(fit)




#######################################################################
#                  Example of Binomial distribution 
#         (data recorded as the # of successes per covariate pattern)
#######################################################################
library(faraway)
data(orings)
?orings
orings

# ---------------------Explore graphically-----------------------------
plot(damage/6 ~ temp, orings, xlim=c(25,85),ylim=c(0,1),
     xlab="Temperature",ylab="Proportion of damage")

# ----------Fit (specify 2 responses: # of 1s and # of 0s)-------------
fit <- glm(cbind(damage, 6-damage) ~ temp, family=binomial, data=orings)
summary(fit)

# --------------Confidence intervals on parameter estimates-------------
library(MASS)
confint(fit)

# ----------------------------Prediction--------------------------------
newOrings <- data.frame(temp=seq(from=10, to=100, length=10))
newOrings.predict <- predict(fit, newdata=newOrings, se.fit=T, type="response")
lines(newOrings$temp, newOrings.predict$fit)

# -------------Bonferroni-corrected CI intervals for the mean-----------
newOrings.predictLink <- predict(fit, newdata=newOrings, se.fit=T, type="link")
L <- newOrings.predictLink$fit - qnorm(1-0.05/(2*10))*newOrings.predictLink$se
U <- newOrings.predictLink$fit + qnorm(1-0.05/(2*10))*newOrings.predictLink$se
lines(newOrings$temp, 1/(1+exp(-L)), lty=2, col="blue")
lines(newOrings$temp, 1/(1+exp(-U)), lty=2, col="blue")

# ---------------------Evidence of mild overdispersion------------------
# Estimate the overdispersion parameter
est.phi <- function(glmobj) { 
  sum( 
    residuals(glmobj, type="pearson")^2 / df.residual(glmobj)
  )
}
est.phi(fit)

# refit the model while allowing for overdispersion
fit1 <- glm(cbind(damage, 6-damage) ~ temp, family=quasibinomial, data=orings)
summary(fit1)

# equivalent (more computationally efficient): update the previous model
summary( update(fit, family="quasibinomial") )

# examine the resulting change in inference
newOrings.predictLink1 <- predict(fit1, newdata=newOrings, se.fit=T, type="link")
L1 <- newOrings.predictLink1$fit - qnorm(1-0.05/(2*10))*newOrings.predictLink1$se
U1 <- newOrings.predictLink1$fit + qnorm(1-0.05/(2*10))*newOrings.predictLink1$se
lines(newOrings$temp, 1/(1+exp(-L1)), lty=2, col="red")
lines(newOrings$temp, 1/(1+exp(-U1)), lty=2, col="red")
legend("topright", lty=2, col=c("blue","red"), c("no overdispersion", "with overdispersion"))

# -------------------------Alternative link function---------------------------
plot(damage/6 ~ temp, orings, xlim=c(25,85),ylim=c(0,1),
     xlab="Temperature",ylab="Proportion of damage")

# Same as before: redo precistion with logit link
newOrings <- data.frame(temp=seq(from=10, to=100, length=10))
newOrings.predict.logistic <- predict(fit, newdata=newOrings, se.fit=T, type="response")
lines(newOrings$temp, newOrings.predict.logistic$fit, col="blue")

# Compare with probit link
fit2 <- glm(cbind(damage,6-damage) ~ temp, family=binomial(link="probit"), data=orings)
newOrings.predict.probit <- predict(fit2, newdata=newOrings, se.fit=T, type="response")
lines(newOrings$temp, newOrings.predict.probit$fit, col="red")

legend("topright", lty=1, col=c("blue", "red"), c("logit", "probit"))

# a more detailed look into the differences
plot(newOrings$temp, newOrings.predict.logistic$fit/newOrings.predict.probit$fit, 
     type="l", xlab="Temperature", ylab="Ratio of predicted probabilities")




#######################################################################
#                   Example of variable selection
#######################################################################
library(faraway)
data(pima)
?pima
head(pima)

# ---------------------Fit the full model-----------------------------
fit <- glm(test ~., family=binomial, data=pima)
summary(fit)


# -------------------------Residual diagnostics-------------------------
# Pearson residuals vs predicted response
plot( residuals(fit, type="pearson") ~ predict(fit, type="response"), 
      xlab=expression(hat(pi)), ylab="Pearson Residual")

# Pearson residuals vs predicted link    
plot(residuals(fit, type="pearson") ~ predict(fit,type="link"), 
     xlab=expression(hat(eta)), ylab="Pearson Residual")

# Deviance residuals vs predicted response
plot( residuals(fit, type="deviance") ~ predict(fit, type="response"), 
      xlab=expression(hat(pi)), ylab="Deviance Residual")

# Studentized residuals vs predicted response
plot( rstudent(fit) ~ predict(fit, type="response"), 
      xlab=expression(hat(pi)), ylab="Studentized Residual")

# Cooks distance
plot(cooks.distance(fit) ~ predict(fit,type="response"), 
     xlab=expression(hat(pi)), ylab="Cooks distance")


#------------------------Deviance test of lack of fit--------------------
# Fail to reject H0 (warning: these are not grouped data - poor quality of approximation!)
pchisq(deviance(fit), df.residual(fit), lower=F)

# H-L test of lack of fit
hosmerlem <- function (y, yhat, g = 10) 
{
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
                                                           1, 1/g)), include.lowest = T)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
hosmerlem(y=pima$test, predict(fit, type="response"), g = 10)



# -------------------------Compare nested models-------------------------
summary(fit)

# let us try to remove 'age' by comparing residual deviances to chisq(1)
fit1 <- update(fit, .~.-age) 
summary(fit1)
pchisq(deviance(fit1)-deviance(fit), 1, lower=F)

# let us try to remove one predictor at a time
# by comparing residual deviances to chisq(1)
drop1(fit, test="Chisq")

# stepwise variable selection based on AIC 
# ('k' distinguishes AIC and BIC)
step.aic <- step(fit, k=2, trace=F) 
step.aic$anova

# stepwise variable selection based on BIC
step.bic <- step(fit, k=log(nrow(pima)), trace=F) 
step.bic$anova

# based on F test, in presence of dispersion
drop1(fit, test="F")



#######################################################################
#                  ROC curves
#######################################################################
# --------------Predictive ability on the training set-------------------------
library(ROCR)
# select training set (as example, here only use 1/4 of the data to build the model)
train <- sample(x=1:nrow(pima), size=nrow(pima)/4)

# fit the full model on the training dataset
fit.train <- glm(test ~., family=binomial, data=pima[train,])
summary(fit.train)
summary(fit)

# calculate predicted probabilities on the same training set
scores <- predict(fit.train, newdata=pima[train,], type="response")

# compare predicted probabilities to labels, for varying probability cutoffs
pred <- prediction(scores, labels=pima[train,]$test )
perf <- performance(pred, "tpr", "fpr")

# plot the ROC curve
plot(perf, colorize=F, main="In-sample ROC curve")

# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)


# --------------Evalate the predictive ability on the validation set------------
# make prediction on the validation dataset
scores <- predict(fit.train, newdata=pima[-train,], type="response")
pred <- prediction( scores, labels=pima[-train,]$test )
perf <- performance(pred, "tpr", "fpr")

# overlay the line for the ROC curve
plot(perf, colorize=T, add=TRUE)

# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)
