## Setup first

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises/Ex9")

# Read in the data
myData <- read.table("VertebralData.2C.txt", header = T, sep=",")
summary(myData)

## 1. Logistic Regression

# Logistic regression
glm.fit=glm(Status~., data=myData, family=binomial)
summary(glm.fit)

# Get predictions
glm.probs=predict(glm.fit, type="response") 
glm.pred=ifelse(glm.probs>0.5,"Normal","Abnormal")

# Confusion matrix
table(glm.pred, myData$Status)
mean(glm.pred==myData$Status)

## 2. LDA

require(MASS)

## Linear Discriminant Analysis
lda.fit=lda(Status~.,data=myData)
lda.fit
plot(lda.fit)

# Get predictions
lda.pred=predict(lda.fit, myData)

# Confusion matrix
table(lda.pred$class, myData$Status)
mean(lda.pred$class==myData$Status)

## 3. Comparison

# Cross validation for general classification models
cross_validate <- function(trainFun, predFun, X, Y, k=10){
  set.seed(123)  # Want reproducability (also for paired t-test!)
  n = nrow(X)
  n_fold = floor(n/k)
  #Define indices for folds
  shuffle = sample(c(rep(seq(k-1),n_fold),rep(k,n-n_fold*(k-1))))
  error = rep(0, k)
  for (i in seq(k)){
    ind = which(shuffle==i)
    model = trainFun(X[-ind,])
    pred = predFun(model, X[ind,])
    # Compute mean of squred residuals MSR
    error[i] = mean(pred!=Y[ind])
  }
  return(error)
}

## Logistic Regression CV

trainFun = function(X){
  return(glm(Status~., data=X, family=binomial))
}
predFun = function(model, X){
  probs = predict(model, newdata = X, type="response") 
  pred = ifelse(probs>0.5, "Normal", "Abnormal")
}

glm.errors = cross_validate(trainFun, predFun, myData, myData$Status)
mean(glm.errors)

## LDA CV

trainFun = function(X){
  return(lda(Status~., data=X))
}
predFun = function(model, X){
  pred = predict(model, X)
  return(pred$class)
}

lda.errors = cross_validate(trainFun, predFun, myData, myData$Status)
mean(lda.errors)

# Comparing the error-rates using paired t-test
t.test(glm.errors, lda.errors, paired=TRUE, alternative="two.sided")

