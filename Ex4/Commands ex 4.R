setwd("G:/SLM EXERCISES/Exercise 4")

setwd("/Users/barbir/Desktop/SLM/Exercise 4")

# QUESTION 1
myData <- read.table("EducationBis.txt", header=T)
summary(myData)

# Remove ID
usefulData <- myData[,-1]
attach(usefulData)
men <- Gender == "male"
women <- Gender == "female"

modMen.lm <- lm(Wage[men] ~ Education[men], data=usefulData)
summary(modMen.lm)

modWomen.lm <- lm(Wage[women] ~ Education[women], data=usefulData)
summary(modWomen.lm)
detach(usefulData)

# QUESTION 2
#   Computer data:  need to predict PRP (and ignore ERP)

myData <- read.table("ComputerData.txt", header=T)
dim(myData)
names(myData)

# We can remove the variable model (name), vendor (name) and ERP
usefulData <- myData[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX", "PRP")] 
predictors <- names(usefulData)

summary(usefulData) # Always check the data
attach(usefulData)

# QUESTION 3
nbP <- 6
modelQuality <- numeric(nbP)
nb <- dim(usefulData)[1]

# Compute the mean RSS as the criterion to select the best predictor
for (i in 1:nbP) {
  computer.lm <- lm(PRP ~ usefulData[,i], data=usefulData)
  epsilon <- computer.lm$residuals
  MRS <- sum(epsilon^2) / nb
  modelQuality[i] <- MRS
  #  a direct form is:    modelQuality[i] <- mean(epsilon^2)
  cat(predictors[i], MRS,"\n")
}
which(modelQuality == min(modelQuality))
predictors[which(modelQuality == min(modelQuality))]  
# Then single best predictor is MMAX


# QUESTION 4
computer.lm <- lm(PRP ~ usefulData[,"MMAX"], data=usefulData)
summary(computer.lm)

plot(PRP ~ usefulData[,"MMAX"], main="Computer performance explained by MMAX",
     xlab="Main memory size (max)", ylab="PRP (Computer system performance)")
abline(computer.lm)


# QUESTION 5
#   Cars2 data:  need to predict mpg

myData <- read.table("Cars2Data.txt", header=T)
dim(myData)
names(myData)
attach(myData)
summary(myData)

# We can remove the variable name 
usefulData <- myData[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year",         "origin")] 
predictors <- names(usefulData)

summary(usefulData) # Check the data


# QUESTION 6
modelQuality <- rep(0,7)
for (i in 2:8) {
  cars.lm <- lm(mpg ~ usefulData[,i], data=usefulData)
  RSS <- mean(cars.lm$residuals^2)
  modelQuality[i-1] <- RSS
  #  a little bit long   summary(computer.lm)
}

which(modelQuality == min(modelQuality))
predictors[which(modelQuality == min(modelQuality))+1]  
# Single best predictor is WEIGHT

cars.lm <- lm(mpg ~ usefulData[,"weight"], data=usefulData)
summary(cars.lm)
confint(cars.lm)

dev.off()
# QUESTION 7
plot(mpg ~ usefulData[,"weight"], main="Car performance explained by the weight", xlab="Car weight", ylab="Car performance (MPG: Miles Per Gallon)")

v <- c(1500:5000)
w <- cars.lm$coefficients[1] + cars.lm$coefficients[2]*v
lines(v,w, col = "green") # Or simply: abline(cars.lm)



