## Setup first

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises/Ex2")
original_data <- read.table("Mean20.txt", header = T)
# We remove outliers as in ex2
my_data = original_data[which(original_data$time > 0), ]

## Ex3.1
secondMax <- function(x) {
  if (is.vector(x)) {
    sorted = sort(x, decreasing = TRUE)
    return(sorted[2])
  } else {
    stop("Input is not a vector!")
  }
}

## Test secondMax
stopifnot(secondMax(c(1, 9, 3, 6)) == 6)
stopifnot(secondMax(c(-1, -9, 3, 6)) == 3)
stopifnot(secondMax(c("hello", "salut", "ciao", "hallo")) == "hello")
stopifnot(secondMax(my_data) == 7.12)
res = try(secondMax(original_data))
stopifnot(class(res) == "try-error") # original_data is dataframe => Expect error

## Ex3.2
mySummary <- function(x) {
  if (is.vector(x)) {
    return(c(
      mean = mean(x),
      median = median(x),
      std = sd(x),
      min = min(x),
      max = max(x)
    ))
  } else {
    # Also show error if unput is not a vetor
    stop("Input is not a vector!")
  }
}

## Test mySummary
stopifnot(mySummary(c(1, 1)) == c(1, 1, 0, 1, 1))
stopifnot(mySummary(c(1, 5, 9)) == c(5, 5, 4, 1, 9))
stopifnot(mySummary(my_data) == c(
  mean(my_data),
  median(my_data),
  sd(my_data),
  min(my_data),
  max(my_data)
))

## Ex3.3
mu = 2
std = 1.5
numSamples = 20
numVals = 20

# Generate a matrix containing all the samples (row~sample)
mat = matrix(rnorm(numSamples * numVals, mean = mu, sd = std),
             nrow = numSamples,
             ncol = numVals)

# Compute t-test for all the samples (each row)
res = apply(mat, 1, function(x)
  t.test(x, mu = 1.5))

# Extract p-values
pVals = lapply(res, function(x)
  x$p.value) #extract p-values

# Count the number of correct results (true mean != 1.5)
numRight = sum(pVals < 0.05)

# Compute t-test on all the sample-values
t.test(as.vector(mat), mu = 1.5)
