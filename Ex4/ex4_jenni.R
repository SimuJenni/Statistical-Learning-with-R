## Setup first

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises/Ex4")

# Read in the data
my_data <- read.table("EducationBis.txt", header = T)

# Check data
summary(my_data)

## Ex1

# Helper function for fitting and plotting
fitAndPlot <- function(fit_data, title) {
  plot(Wage ~ Education, fit_data, main = title)
  fit = lm(Wage ~ Education, data = fit_data)
  abline(fit, col = "red")
  fit
}

# Seperating the data according to gender
male_data = my_data[which(my_data$Gender == "female"), ]
female_data = my_data[which(my_data$Gender == "male"), ]

# Fit the model
fit_male = fitAndPlot(male_data, "Male Data")
fit_female = fitAndPlot(female_data, "Female Data")
summary(fit_male)
summary(fit_female)

## Ex2

# Read in the data
my_data <- read.table("ComputerData.txt", header = T)

summary(my_data)
# Looking at the description of the data, the following attributes can not be used to predict the performance:
# - vendor & model which are non-descriptive
# - PRP: the goal field
# - ERP: the linear regression's guess

fit = lm(PRP ~ MYCT + MMIN + MMAX + CACH + CGMIN + CHMAX, data = my_data)
summary(fit)
# We observe that all but CGMIN are significant in estimating PRP

## Ex3

# We use MMAX as it showed to be the most significant attribute in the previous exercise (lowest p-value)
fit = lm(PRP ~ MMAX, data = my_data)
summary(fit)
confint(fit)
# The slope we estimated is positive but rather small (1.184e-02).
# However, looking at the confidence-interval we are confident that PRP increases linearly with MMAX

## Ex4

plot(PRP ~ MMAX, data = my_data, main="Relationship between PRP and MMAX")
abline(fit, col = "red")

## Ex5

# Read in the data
my_data <- read.table("Cars2Data.txt", header = T)

summary(my_data)
# As there are 6 NA's in the variable horsepower, we will remove these samples from the data
clean_data = my_data[which(!is.na(my_data$horsepower)),]

# The name of the car cannot be used because if we would use it, our model would not be able
# to predict mpg for any previously unseen model. It is still intresting to observe what happens
# when we incorporate the name in the fit however:
fit = lm(mpg ~., data = clean_data)
summary(fit)
# We observe that in cases where the name contains (diesel), the name is actually very significant.

# It is a bit unclear how origin is defined, but it seems like it indicates whether the car was 
# produced in USA (1), Asia (3) or Europe (2). As long as we do not want to predict mpg for cars
# falling originating from outside of these zones (whatever that might be) and therefore assume 
# that all cars can be classified in origin 1, 2 or 3, it makes sense to use this attribute.


## Ex6

# Fit without name
fit = lm(mpg ~.-name, data = clean_data)
summary(fit)

# We use weight as it showed to be the most significant attribute (lowest p-value)
fit = lm(mpg ~ weight, data = clean_data)
summary(fit)
confint(fit)
# We estimated a small negative slope of -0.007647
# Looking at the confidence-interval we are confident that mpg decreases linearly with weight

## Ex7

plot(mpg ~ weight, data = clean_data, main="Relationship between weight and mpg")
abline(fit, col = "red")
