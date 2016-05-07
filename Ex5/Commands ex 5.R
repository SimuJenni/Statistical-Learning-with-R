setwd("/Users/barbir/Desktop/SLM/Exercise 4")
# Exercise #5
#
#   Computer data:  need to predict PRP (and ignore ERP)
#
myData <- read.table("ComputerData.txt", header=T)
attach(myData)
#
# we can remove the variable model (name), vendor (name) and ERP
#
usefulData <- myData[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX", "PRP")] 
predictors <- names(usefulData)
summary(usefulData) # always check the data


modelQuality <- rep(1000000, 50)
modelDesc <- rep("", 50)
nb <- dim(usefulData)[1]
nbP <- length(predictors)-1
index <- 1
#
# a little bit long 
#
for (i in 1:6) {
  computer.lm <- lm(PRP ~ usefulData[,i], data=usefulData)
  MRS <- mean(computer.lm$residuals^2)
  modelQuality[index] <- MRS
  modelDesc[index] <- predictors[i]
  cat(index, predictors[i], MRS,"\n")
  index <- index + 1
  if ((i+1) < 6) {
    for (j in (i+1):6) {
      computer.lm <- lm(PRP ~ usefulData[,i]+usefulData[,j], data=usefulData)
      MRS <- mean(computer.lm$residuals^2)
      modelQuality[index] <- MRS
      cat(index, predictors[i], predictors[j], MRS,"\n")
      modelDesc[index] <- paste(predictors[i], " ", predictors[j])
      index <- index + 1
      if ((j+1) < 7) {
        for (k in (j+1):6) {
          computer.lm <- lm(PRP ~ usefulData[,i]+usefulData[,j]+usefulData[,k], data=usefulData)
          MRS <- mean(computer.lm$residuals^2)
          modelQuality[index] <- MRS
          cat(index, predictors[i], predictors[j], predictors[k], MRS,"\n")
          modelDesc[index] <- paste(predictors[i], " ", predictors[j], " ", predictors[k])
          index <- index + 1
        }
      }
    } }
}
which(modelQuality == min(modelQuality))
modelDesc[which(modelQuality == min(modelQuality))]  


#
#  single best predictor is MMAX
#  with triple predictors, the best solution is MMIN   MMAX   CHMAX

computer.lm1 <- lm(PRP ~ usefulData[,"MMAX"], data=usefulData)
summary(computer.lm1)   # R^2 74.35
computer.lm3 <- lm(PRP ~ MMIN+MMAX+CHMAX, data=usefulData)
summary(computer.lm3)  # R^2 84.44

#
# a more direct solution
# focus to minimize AIC
# add one variable at each step
# the final model
# PRP ~ MMAX + CACH + MMIN + CHMAX + MYCT
#
step(lm(PRP~1, data=usefulData), direction="forward", scope=~MYCT+MMIN+MMAX+CACH+CGMIN+CHMAX) 

#
# with the backward option
# PRP ~ MYCT + MMIN + MMAX + CACH + CHMAX
#
step(lm(PRP~., data=usefulData), direction="backward", scope=~MYCT+MMIN+MMAX+CACH+CGMIN+CHMAX) 
#
#  step() is similar to stepAIC)= in the package MASS
#

#
#
#

computer.lm <- lm(PRP ~ usefulData[,"MMAX"], data=usefulData)

pdf(file="Ex3a.pdf")

plot(PRP ~ usefulData[,"MMAX"], main="Computer performance explained by MMAX",
     xlab="Main memory size (max)", ylab="PRP (Computer system performance)")
abline(computer.lm1)
#
#  the largest epsilon value
#
epsilon <- computer.lm$residuals
maxResiduals <- max(abs(epsilon))
index <- which(abs(epsilon) == max(abs(epsilon)))
x0 <- usefulData[index,"MMAX"]
y0 <- usefulData[index,"PRP"]
y0hat <-  computer.lm$fitted.values[index]
segments(x0, y0, x0, y0hat, col="red", pch=20)   # a line indicating the max epsilon
points(x0, y0, col="black", pch=20)           # a point for the hat{y}

dev.off()


#
#   Cars2 data:  need to predict mpg
#
myData <- read.table("Cars2Data.txt", header=T)
subset <- !(is.na(myData[,"horsepower"]))
#
# we can remove the variable name to infer mpg
#
usefulData <- myData[subset, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")] 
predictors <- names(usefulData)
attach(usefulData)

#
# always check the data???
#
summary(usefulData)

#
# linear with max 3 predictors
# the long version
#
modelQuality <- rep(1000000, 50)
modelDesc <- rep("", 50)
nb <- dim(usefulData)[1]
nbP <- length(predictors)
index <- 1
for (i in 2:nbP) {
  computer.lm <- lm(mpg ~ usefulData[,i], data=usefulData)
  MRS <- mean(computer.lm$residuals^2)
  modelQuality[index] <- MRS
  modelDesc[index] <- predictors[i]
  cat(index, predictors[i], MRS,"\n")
  index <- index + 1
  
  if ((i+1) < (nbP+1)) {
    for (j in (i+1):nbP) {
      computer.lm <- lm(mpg ~ usefulData[,i]+usefulData[,j], data=usefulData)
      MRS <- mean(computer.lm$residuals^2)
      modelQuality[index] <- MRS
      cat(index, predictors[i], predictors[j], MRS,"\n")
      modelDesc[index] <- paste(predictors[i], " ", predictors[j])
      index <- index + 1
      
      if ((j+1) < (nbP+1)) {
        for (k in (j+1):nbP) {
          computer.lm <- lm(mpg ~ usefulData[,i]+usefulData[,j]+usefulData[,k], data=usefulData)
          MRS <- mean(computer.lm$residuals^2)
          modelQuality[index] <- MRS
          cat(index, predictors[i], predictors[j], predictors[k], MRS,"\n")
          modelDesc[index] <- paste(predictors[i], " ", predictors[j], " ", predictors[k])
          index <- index + 1
        }
      }
    }
  }
}
which(modelQuality == min(modelQuality))
modelDesc[which(modelQuality == min(modelQuality))]  

# ignoring year
# best model is "cylinders   displacement   weight"
# OR
# best model is "weight   acceleration   year"


#
# linear with max 3 predictors
#
modelQuality <- rep(1000000, 50)
modelDesc <- rep("", 50)
nb <- dim(usefulData)[1]
nbP <- length(predictors)
index <- 1
for (i in 2:nbP) {
  computer.lm <- lm(mpg ~ usefulData[,i], data=usefulData)
  MRS <- mean(computer.lm$residuals^2)
  modelQuality[index] <- MRS
  modelDesc[index] <- predictors[i]
  cat(index, predictors[i], MRS,"\n")
  index <- index + 1
  
  computer.lm <- lm(mpg ~ usefulData[,i]+I(usefulData[,i]^2), data=usefulData)
  MRS <- mean(computer.lm$residuals^2)
  modelQuality[index] <- MRS
  modelDesc[index] <- paste(predictors[i], " ", predictors[i], "^2")
  cat(index, predictors[i], predictors[i], "^2", MRS,"\n")
  index <- index + 1
  
  if ((i+1) < (nbP+1)) {
    for (j in (i+1):nbP) {
      computer.lm <- lm(mpg ~ usefulData[,i]+usefulData[,j], data=usefulData)
      MRS <- mean(computer.lm$residuals^2)
      modelQuality[index] <- MRS
      cat(index, predictors[i], predictors[j], MRS,"\n")
      modelDesc[index] <- paste(predictors[i], " ", predictors[j])
      index <- index + 1
      
      computer.lm <- lm(mpg ~ usefulData[,i]+usefulData[,j]+I(usefulData[,j]^2), data=usefulData)
      MRS <- mean(computer.lm$residuals^2)
      modelQuality[index] <- MRS
      cat(index, predictors[i], predictors[j], predictors[j], "^2", MRS,"\n")
      modelDesc[index] <- paste(predictors[i], " ", predictors[j], " ", predictors[j],"^2")
      index <- index + 1
      
      if ((j+1) < (nbP+1)) {
        for (k in (j+1):nbP) {
          computer.lm <- lm(mpg ~ usefulData[,i]+usefulData[,j]+usefulData[,k], data=usefulData)
          MRS <- mean(computer.lm$residuals^2)
          modelQuality[index] <- MRS
          cat(index, predictors[i], predictors[j], predictors[k], MRS,"\n")
          modelDesc[index] <- paste(predictors[i], " ", predictors[j], predictors[k])
          index <- index + 1
        }
      }
    }
  }
}
which(modelQuality == min(modelQuality))
modelDesc[which(modelQuality == min(modelQuality))]  

#
#  best predictors are "displacement   horsepower   horsepower ^2"
#
#  with year, we have:
#  best predictors are "weight   year   year ^2"
#

car.lm1 <- lm(mpg ~ weight, data=usefulData)
summary(car.lm1)  # R^2 69.10
car.lm3a <- lm(mpg ~ weight+cylinders+displacement, data=usefulData)
summary(car.lm3a)  # R^2 69.59
car.lm3b <- lm(mpg ~ weight+horsepower+I(horsepower^2), data=usefulData)
summary(car.lm3b)  # R^2 73.76

car.lm3c <- lm(mpg ~ displacement+horsepower+I(horsepower^2), data=usefulData)
summary(car.lm3c)  # R^2 72.46

car.lm3d <- lm(mpg ~ weight+year+I(year^2), data=usefulData)
summary(car.lm3d)  # R^2 81.79

car.lm2b <- lm(mpg ~ horsepower+I(horsepower^2), data=usefulData)
summary(car.lm2b)  # R^2 68.6

#
#  plot mpg ~ horsepower^2 + horsepower
#

car.lm <- lm(mpg ~ horsepower+I(horsepower^2), data=usefulData)

pdf(file="Ex4b.pdf")

plot(mpg ~ usefulData[,"horsepower"], main="Car performance explained by the horsepower + horsepower^2",
     xlab="horsepower", ylab="mpg (Car performance, miles per gallon)")

xOrder <- order(usefulData[,"horsepower"]) 
x <- usefulData[,"horsepower"]
y <- car.lm$fitted
lines(x[xOrder], y[xOrder], col="red") 
#
#  the largest epsilon value
#
epsilon <- computer.lm$residuals
maxResiduals <- max(abs(epsilon))
index <- which(abs(epsilon) == max(abs(epsilon)))[1]
x0 <- usefulData[index,"horsepower"]
y0 <- usefulData[index,"mpg"]
y0hat <-  car.lm$fitted.values[index]
segments(x0, y0, x0, y0hat, col="red", pch=20)   # a line indicating the max epsilon
points(x0, y0, col="black", pch=20)           # a point for the hat{y}

dev.off()

car.lm <- lm(mpg ~ year+I(year^2), data=usefulData)

pdf(file="Ex4b2.pdf")

plot(mpg ~ usefulData[,"year"], main="Car performance explained by the year + year^2",
     xlab="year", ylab="mpg (Car performance, miles per gallon)")

xOrder <- order(usefulData[,"year"]) 
x <- usefulData[,"year"]
y <- car.lm$fitted
lines(x[xOrder], y[xOrder], col="red") 
#
#  the largest epsilon value
#
epsilon <- computer.lm$residuals
maxResiduals <- max(abs(epsilon))
index <- which(abs(epsilon) == max(abs(epsilon)))[1]
x0 <- usefulData[index,"year"]
y0 <- usefulData[index,"mpg"]
y0hat <-  car.lm$fitted.values[index]
segments(x0, y0, x0, y0hat, col="red", pch=20)   # a line indicating the max epsilon
points(x0, y0, col="black", pch=20)           # a point for the hat{y}

dev.off()

