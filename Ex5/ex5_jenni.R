## Setup first

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises/Ex4")

## Ex1

# Read in the data
my_data <- read.table("EducationBis.txt", header = T)
summary(my_data)

# Linear regression predicting Wage from all other attributes
fit = lm(Wage ~ ., data = my_data)
summary(fit)
# As can be expected we observe that ID is not significant while
# the other variables are clearly significant. 
# Based on the t-value, we see that Education is the most 
# significant predictor. 

# Plot the fit
plot(fit)
# Residual plot indicates that the linear model fits well

## Ex2

# Function that performs forward variable selection
forwardSelection <- function(df, targetVar, maxIter=10000){
  vars = names(df)
  vars = vars[vars != targetVar]
  # Initial model
  f <- paste(targetVar, "~ 1")
  bestFit = lm(as.formula(f), data = df)
  chosen = list()
  done = FALSE
  iter = 0;
  while (!done) {
    rss = list()
    for (var in vars) {
      # Update model
      tmpFit = update(bestFit,as.formula(paste("~.+",var)))
      rss = c(rss, summary(tmpFit)[[6]]) 
    }
    # Choose variable with minimum RSS
    chosenVar = vars[which.min(rss)]
    chosen = c(chosen, chosenVar)
    vars = vars[vars != chosenVar]
    # Compare new fit to previous fit
    newFit = update(bestFit,as.formula(paste("~.+",chosenVar)))
    done = anova(bestFit,newFit)[[6]][2]>0.05 # if P(>F)>0.05
    if (!done) {
      # If significantly better updated model
      bestFit = newFit
    }
    iter = iter + 1
    done = done || (iter >= maxIter)
  }
  return(bestFit)
}


# Read in the data
my_data <- read.table("ComputerData.txt", header = T)

# As discussed in Ex4 will remove:
# - vendor & model which are non-descriptive
# - ERP: the linear regression's guess
drops <- c("ERP","vendor","model")
clean_data = my_data[ , !(names(my_data) %in% drops)]

# Perform forward selection
fit1 = forwardSelection(clean_data, "PRP")
summary(fit1)
plot(fit1)

## Ex3

# Plot of most improtant variable (=> forward with only 1 variable)
fit2 = forwardSelection(clean_data, "PRP", 1)
summary(fit2) # => MMAX most important
plot(PRP~MMAX, data = clean_data)
abline(fit2, col='red')

## Ex4

# Read in the data
my_data <- read.table("Cars2Data.txt", header = T)
summary(my_data)

# As there are 6 NA's in the variable horsepower, we will remove these samples from the data
clean_data = my_data[which(!is.na(my_data$horsepower)),]
# Also drop name as discussed in Ex4
drops <- c("name")
clean_data = clean_data[ , !(names(clean_data) %in% drops)]
# Origin should be categorical -> make it so!
clean_data$origin <- as.factor(clean_data$origin)

# Perform forward selection
fit3 = forwardSelection(clean_data, "mpg")
summary(fit3)
plot(fit3)

# Plot of most improtant variable (=> forward with only 1 variable)
fit4 = forwardSelection(clean_data, "mpg", 1) 
summary(fit4) # => weight is most improtant variable
plot(mpg~weight, data = clean_data)
abline(fit4, col='red')
