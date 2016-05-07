## Setup first

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises/Ex8")

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
    model = glm(formula, data = X[-ind,])
    pred = predict(model, X[ind,])
    # Compute mean of squred residuals MSR
    error[i] = mean((pred-Y[ind])^2)
  }
  return(error)
}

## Ex1

# Read in the data
myData <- read.table("Cars2Data.txt", header = T)
myData = myData[which(!is.na(myData$horsepower)),]
usefulData <- myData[, c("mpg", "cylinders", "displacement", "horsepower",
                         "weight", "year")]

# a)

fml.single = as.formula(paste('mpg~weight'))
fml.quadratic = as.formula(paste('mpg~poly(weight, 2)'))
fml.multi = as.formula(paste('mpg~poly(weight+cylinders+horsepower+year+displacement, 3)'))

# b)

# Use my cross validation implementation to perform paired t-test later...
err.single = cross_validate_lm(k=10, fml.single, usefulData, usefulData$mpg)
err.quadratic = cross_validate_lm(k=10, fml.quadratic, usefulData, usefulData$mpg)
err.multi = cross_validate_lm(k=10, fml.multi, usefulData, usefulData$mpg)
pred.err.single = mean(err.single)
pred.err.quadratic = mean(err.quadratic)
pred.err.multi = mean(err.multi)


# c)

# Comparing the sum of squared residuals using paired t-test
t.test(err.single, err.quadratic, paired=TRUE, alternative="two.sided")
t.test(err.single, err.multi, paired=TRUE, alternative="two.sided")
t.test(err.quadratic, err.multi, paired=TRUE, alternative="two.sided")


## Ex2

# Select useful data
myData <- read.table("cancer.txt", header = T)
usefulData = myData[,c("Diagnostic", "Radius", "Texture", "Perimeter", "Area",
                       "Smooth","Compact", "Concavity", "Concave", "Symmetry", 
                       "Fractal")]
usefulData = myData[,-which(names(myData)=='ID')]


# Divide into train and test set
numSamples = dim(usefulData)[1]
trainIdx = sample(seq(numSamples), 0.8*numSamples)
train = usefulData[trainIdx,]
test = usefulData[-trainIdx,]

# Train model
chd.glm <- glm(Diagnostic~., data = train, family=binomial, control = glm.control(maxit = 100))
summary(chd.glm)

# Get predictions
h = predict(chd.glm, test, type="response") > 0.5
prediction = rep('B', dim(test)[1])
prediction[h] = 'M'

# Compute error-rate
error_rate = mean(prediction!=test$Diagnostic)
