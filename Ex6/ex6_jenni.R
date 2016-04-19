## Setup first

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises/Ex6")

## Ex1

# Read in the data
myData <- read.table("ComputerData.txt", header = T)
usefulData <- myData[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX", "PRP")]

# Function that standardizes the provided data
standardized <- function(usefulData){
  means <- lapply(usefulData, mean) # means per variable 
  sd <- lapply(usefulData, sd) # sd per variable 
  usefulData <- (usefulData - means) / sd
  return(usefulData)
}

# Test standardization
stdData = standardized(usefulData)
means <- apply(stdData, 2, mean)
stds <- apply(stdData, 2, sd)
stopifnot(sum(abs(means)>0.0001)==0)
stopifnot(sum(abs(stds-1)>0.0001)==0)


# Function that computes the distance between two observations
mydist <- function(obs1, obs2, usedNorm = "L2") {
  if (usedNorm == "L2") {
    return(sqrt(sum((obs1-obs2)^2)))
  } else if (usedNorm == "L1") {
    return(sum(abs(obs1-obs2)))
  } else {
    warning("Expected third parameter to be \"L1\" or \"L2\"")
  }
}

# Test dist-function 
var1 = c(2,2)
var2 = c(0,0)
d1 = mydist(var1,var2,"L1")
d2 = mydist(var1,var2,"L2")
stopifnot(d1 == 4)
stopifnot(abs(d2-2*sqrt(2))<0.0001)

## Ex2 Computers

# k-NN using the library
library(FNN)

# Read in the data
myData <- read.table("ComputerData.txt", header = T)

# Divide data into train and test sets
set.seed(123)
n = nrow(myData)
numTrain = round(0.7*n)
trainIdx <- sample(1:n, numTrain)
trgtData <- myData[, c("PRP")]
predData <- standardized(myData[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX")])
trainData = predData[trainIdx,]
testData = predData[-trainIdx,]

# Apply k-NN for different values of k and record Sum Residual Squared
srs = rep(0,10)
for (i in c(1:10)) {
  lib.knn <- knn.reg(train=trainData, test = testData, y=trgtData[trainIdx], k=i)
  srs[i] = sum((lib.knn$pred-trgtData[-trainIdx])^2)
}

# Best k is the one with lowest SRS
bestK = which(srs == min(srs))

lib.knn <- knn.reg(train=trainData, test = testData, y=trgtData[trainIdx], k=bestK)
plot((trgtData[-trainIdx]-lib.knn$pred)^2, ylab="Residual Squared", main="Residual of best k on ComputerData")

## Ex2 Cars

# Read in the data
myData <- read.table("Cars2Data.txt", header = T)
myData = myData[which(!is.na(myData$horsepower)),]

# Divide data into train and test sets
set.seed(123)
n = nrow(myData)
numTrain = round(0.7*n)
trainIdx <- sample(1:n, numTrain)
trgtData <- myData[, c("mpg")]
predData <- standardized(myData[, c("cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])
trainData = predData[trainIdx,]
testData = predData[-trainIdx,]

# Apply k-NN for different values of k and record Sum Residual Squared
srs = rep(0,50)
for (i in c(1:50)) {
  lib.knn <- knn.reg(train=trainData, test = testData, y=trgtData[trainIdx], k=i)
  srs[i] = sum((lib.knn$pred-trgtData[-trainIdx])^2)
}

# Best k is the one with lowest SRS
bestK = which(srs == min(srs))

lib.knn <- knn.reg(train=trainData, test = testData, y=trgtData[trainIdx], k=bestK)
plot((trgtData[-trainIdx]-lib.knn$pred)^2, ylab="Residual Squared", main="Residual of best k on Cars2Data")



## This was probably not required (which I realized a bit late) but anyway:

# Simple k-Nearest Neighbor regression model
kNN <- function(inData, regVar, k=1) {
  trainData = inData[,!(names(inData) %in% c(regVar))]
  # Standardize the training data (need mean and sd for later)
  means <- lapply(trainData, mean) 
  std <- lapply(trainData, sd)  
  trainData <- (trainData - means) / std
  reg <- function(x) {
    # Apply same standardisation as training data!
    x = (x - means)/std
    l2_dist <- function(v, w=x) {
      return(mydist(v,w,"L2"))
    }
    dists <- apply(trainData, 1, l2_dist)  
    sortedDist = sort(dists)
    thresh = sortedDist[k]
    neighbors = inData[dists<=thresh,c(regVar)]
    return(mean(neighbors)) # This should be optimised... 
  }
  return(reg)
}

kNN.model = kNN(usefulData, "PRP", 3)
testData = usefulData[,!(names(usefulData) %in% c("PRP"))]

# Regression with my method (not super fast :D)
pred = rep(0,nrow(testData))
for (i in c(1:nrow(testData))) {
  pred[i] = kNN.model(testData[i,])
}