## Setup first

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises/Ex2")

# Read in the data
original_data <- read.table("Education.txt", header = T)

# Get an overview of the data
summary(original_data)

# We observe some fishy data such as educ<0 or gender!=1|2
fishy = which(original_data$Education < 0 | 
                !original_data$Gender %in% c(1,2) |
                original_data$Wage == min(original_data$Wage))

# We remove the fishy data 
my_data = original_data[-fishy,]
rm(original_data)

## Ex 2.1
plot(my_data$Wage~my_data$Education, main="Education vs. Wage", 
     xlab="Education", ylab="Wage", col="blue", pch=16)
abline(0, 390, col="red") # Adding a line to indicate the relationship


## Ex 2.2

# Seperating the data according to gender
male_data = my_data[which(my_data$Gender==1),]
female_data = my_data[which(my_data$Gender==2),]

plot(male_data$Wage~male_data$Education, main="Education vs. Wage", 
     xlab="Education", ylab="Wage", col="blue", pch=16)
points(female_data$Wage~female_data$Education, col="red", pch=16)
legend(5, 8400, c("male","female"), col=c("blue","red"), pch=c(16,16), text.width=c(3,3))


## Ex 2.3

original_data <- read.table("Mean20.txt", header = T)
summary(original_data)
# - We observe a negative time-value: This could be a typo, faulty data,...
#   Because we don't know we will excude it from the data-set (as suggested 
#   in the last exercise-session)
# - We also observe a NaN which we'll also remove

my_data = original_data[which(original_data$time>0),]
summary(my_data)
std = sd(my_data)

## Ex 2.4

t.test(my_data,mu=7.05,alternative="two.sided")
# Small p-value of 0.02178 -> Reject H_0 (i.e. true mean different from 7.05)

t.test(original_data,mu=7.05,alternative="two.sided")
# Results in large p-value of 0.3 -> Do not reject H_0 (influence of outlier!)

## Ex 2.5

t.test(my_data,mu=7.05,alternative="greater")
# Very large p-value of 0.9891 -> Do not reject H_0
# I would doubt Marys theory with this result though :)

