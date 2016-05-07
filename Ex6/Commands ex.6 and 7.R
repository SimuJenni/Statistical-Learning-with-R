# Compute the distance between two points (default: Euclidean)
# type = {"Euclidian", "Manhattan")
# YOU SHOULD ALSO VERIFY THE INPUT GIVEN BY THE USER TO THE FUNCTION

myDistance <- function(x, y, type="Euclidian") {
  if ((type=="Manhattan") | (type=="L1")) {  # or L1-norm
    dist <- sum( abs(x - y) )
  }
  else {      # Euclidian or L2-norm by default
    dist <- sqrt( sum( (x - y)^2 ))
  }
  return( dist )
}

# Verify the computation
x <- c(2, 4);  y <- c(3, 2)
myDistance(x, y)
sqrt(5)   # must be equal to the previous computation

myDistance(x, y, type="Manhattan") # must be equal to 3

# ----------------------------------------------------------

install.packages("FNN")
library(FNN)

#   Computer data:  need to predict PRP (and ignore ERP)
myData <- read.table("ComputerData.txt", header=T)
attach(myData)
# Remove the variable model (name), vendor (name) and ERP
usefulData <- myData[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX", "PRP")] 
predictors <- names(usefulData)
# Standardized the values (Z score)
means <- lapply(usefulData, mean)
sd <- lapply(usefulData, sd)
usefulData <- (usefulData - means) / sd
summary(usefulData)

# compute the performance (Using Leaving-One-Out) 
for (i in 1:11) {
  Computer.knn <- knn.reg(usefulData, test=NULL, PRP, k = i)
  cat("k: ", i, "press: ", Computer.knn$PRESS, "R^2: ", Computer.knn$R2Pred,"\n")
}
# k= 1 the best choice for the Computer Data

#
#   Cars2 data:  need to predict mpg
#
myData <- read.table("Cars2Data.txt", header=T)
subset <- !(is.na(myData [,"horsepower"]))
# Remove the variable name to infer mpg
usefulData <- myData[subset, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration")] 
predictors <- names(usefulData)
attach(usefulData)

# and repeat the previous computation (Using Leaving-One-Out)  
for (i in 1:11) {
  Car.knn <- knn.reg(usefulData, test=NULL, mpg, k = i)
  cat("k: ", i, "press: ", Car.knn$PRESS, "R^2: ", Car.knn$R2Pred,"\n")
}


#  Multiple regression:  the best model is
car.lm3 <- lm(mpg ~ weight+horsepower+I(horsepower^2), data=usefulData)
summary(car.lm3)  # R^2 73.76


# Comparing the k-nn and multiple regression

# Simple strategy:  repeated hold-out
nb <- dim(usefulData)[1]
nbTest <- 10
diffKnn <- numeric(nbTest)
diffLM <- numeric(nbTest)
for (i in 1:nbTest) {   # ten repeated holdout
  subset <- sample(1:nb, 40)
  trainData <- usefulData[-subset,]
  testData <- usefulData[subset,]
  car.knn <- knn.reg(trainData, test=testData, mpg, k=5)
  diffKnn[i] <- sum (abs(car.knn$pred - testData$mpg))
  car.lm <- lm(mpg ~ weight+horsepower+I(horsepower^2), data=trainData)
  car.lm.predict <- predict(car.lm,testData)
  diffLM[i] <- sum (abs(car.lm.predict  - testData$mpg))
}
cat("Abs difference knn:", sum(diffKnn)," linear reg:",sum(diffLM)," \n")  

