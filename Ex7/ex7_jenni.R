## Setup first

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises/Ex7")

## Ex1

# Read in the data
myData <- read.table("ComputerData.txt", header = T)
usefulData <- myData[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX", "PRP")]

## a)

# Cross validation for linear regression model
cross_validate_lm <- function(k = 10, formula, X, Y){
  set.seed(123)  # Want reproducability (also for paired t-test!)
  n = nrow(X)
  n_fold = floor(n/k)
  #Define indices for folds
  shuffle = sample(c(rep(seq(k-1),n_fold),rep(k,n-n_fold*(k-1))))
  error = rep(0, k)
  for (i in seq(k)){
    ind = which(shuffle==i)
    model = lm(formula, data = X[-ind,])
    pred = predict(model, X[ind,])
    # Compute mean of squred residuals MSR
    error[i] = mean((pred-Y[ind])^2)
  }
  return(error)
}

library(DAAG)

# # Best single linear regression model as found in Assignement 4
# mod.single = lm(PRP~MMAX, data = usefulData)
# 
# # Best multiple linear regression model as found in Assignment 5
# mod.multy = lm(PRP~MMAX+CACH+MMIN+CHMAX+MYCT, data = usefulData)
# 
# # Perform 10-fold cross validation for both models
# cv.single = cv.lm(data = usefulData, form.lm = mod.single, m = 10, seed = 123)
# cv.multy = cv.lm(data = usefulData, form.lm = mod.multy, m = 10, seed = 123)

# Best single linear regression model as found in Assignement 4
err.single = cross_validate_lm(k=10, as.formula(paste('PRP~MMAX')), usefulData, usefulData$PRP)

# Best multiple linear regression model as found in Assignment 5
err.multy = cross_validate_lm(k=10, as.formula(paste('PRP~MMAX+CACH+MMIN+CHMAX+MYCT')), usefulData, usefulData$PRP)

# Comparing the sum of squared residuals using paired t-test
t.test(err.single, err.multy, paired=TRUE, alternative="two.sided")

# Plotting the MSR
plot(err.single,type="p",col="red", pch = 1, main = "MSR per Fold on ComputerData", ylab = "MSR", 
     xlab = "fold", xlim = c(1,10), ylim = c(0, 1.2*max(max(err.multy),max(err.single))))
points(err.multy, col = "blue", pch = 2)
legend("topright", c("single", "multy"), inset = 0.05, pch = c(1,2), col = c("red", "blue"))


# b)

# k-NN using the library
library(FNN)

# Cross validation for knn
cross_validate_knn <- function(k=10,ks,X,Y){
  set.seed(123)  # Want reproducability (also for paired t-test!)
  n = nrow(X)
  n_fold = floor(n/k)
  #Define indices for folds
  shuffle = sample(c(rep(seq(k-1),n_fold),rep(k,n-n_fold*(k-1))))
  error = matrix(rep(0,k*length(ks)),nrow=k)
  for (i in seq(k)){
    ind = which(shuffle==i)
    # Normalise data 
    means <- lapply(X[-ind,], mean) # means per variable 
    sd <- lapply(X[-ind,], sd) # sd per variable 
    train_X <- (X[-ind,] - means) / sd
    test_X <- (X[ind,] - means) / sd
    train_Y = (Y[-ind]-mean(Y[-ind]))/sd(Y[-ind])
    for (j in seq(length(ks))){
      pred = knn.reg(train_X, test = test_X, train_Y, k = ks[j],
                     algorithm="kd_tree")$pred
      # Compute mean of squred residuals MSR
      pred = pred*sd(Y[-ind])+mean(Y[-ind])
      error[i,j] = mean((pred-Y[ind])^2)
    }
  }
  return(error)
}

err = cross_validate_knn(10, seq(20), usefulData, usefulData$PRP)
# Mean over all folds per k
err_k = colMeans(err)
# Best model is the one with lowest MSR 
best_k = which(err_k == min(err_k), arr.ind = TRUE)

# Plotting the MSR
plot(err_k,type="o",col="blue", main = "k-NN: Average MSR per k on ComputerData",
     ylab = "MSR", xlab = "k")

## c)

# Comparing the sum of squared residuals using paired t-test
err.knn = err[,best_k]
t.test(err.knn, err.multy, paired=TRUE, alternative="two.sided")

# Plotting the MSR
plot(err.knn,type="p",col="red", pch = 3, main = "MSR per Fold on Cars2Data", ylab = "MSR", 
     xlab = "fold", xlim = c(1,10), ylim = c(0, 1.2*max(max(err.multy),max(err.knn))))
points(err.multy, col = "blue", pch = 4)
legend("topright", c("k-NN", "multy"), inset = 0.05, pch = c(3,4), col = c("red", "blue"))


## Ex2

# Read in the data
myData <- read.table("Cars2Data.txt", header = T)
myData = myData[which(!is.na(myData$horsepower)),]
usefulData <- myData[, c("mpg", "cylinders", "displacement", "horsepower",
                         "weight", "year")]

# a)

# Best single linear regression model as found in Assignement 4
err.single = cross_validate_lm(k=10, as.formula(paste('mpg~weight')), usefulData, usefulData$mpg)

# Best multiple linear regression model as found in Assignment 5 (-origin)
err.multy = cross_validate_lm(k=10, as.formula(paste('mpg~weight+year')), usefulData, usefulData$mpg)

# Comparing the sum of squared residuals using paired t-test
t.test(err.single, err.multy, paired=TRUE, alternative="two.sided")

# Plotting the MSR
plot(err.single,type="p",col="red", pch = 1, main = "MSR per Fold on Cars2Data", ylab = "MSR", 
     xlab = "fold", xlim = c(1,10), ylim = c(0, 1.2*max(max(err.multy),max(err.single))))
points(err.multy, col = "blue", pch = 2)
legend("topright", c("single", "multy"), inset = 0.05, pch = c(1,2), col = c("red", "blue"))


# b)

err = cross_validate_knn(10, seq(20), usefulData, usefulData$mpg)
# Sum over all folds
err_k = colSums(err)
# Best model is the one with lowest MSR 
best_k = which(err_k == min(err_k), arr.ind = TRUE)

# Plotting the MSR
plot(err_k,type="o",col="blue", main = "k-NN: Average MSR per k on Cars2Data",
     ylab = "MSR", xlab = "k")

# c)

# Comparing the sum of squared residuals using paired t-test
err.knn = err[,best_k]
t.test(err.knn, err.multy, paired=TRUE, alternative="two.sided")

# Plotting the MSR
plot(err.knn,type="p",col="red", pch = 3, main = "MSR per Fold on Cars2Data", ylab = "MSR", 
     xlab = "fold", xlim = c(1,10), ylim = c(0, 1.3*max(max(err.multy),max(err.knn))))
points(err.multy, col = "blue", pch = 4)
legend("topright", c("k-NN", "multy"), inset = 0.05, pch = c(3,4), col = c("red", "blue"))


