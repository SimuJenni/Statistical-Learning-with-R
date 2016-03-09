## Ex 1.1

# Change directory to where data is stored
setwd("~/Dropbox/Study/Statistical Learning/Exercises")

# Read in the data
val <- read.table("Education.txt", header = T)

# An all-in-one approach :)
summary(val)

# We observe some fishy data such as educ<0 or gender!=1|2
fishy = which(val$Education < 0 | !val$Gender %in% c(1,2) )

# We remove the fishy data 
val = val[-fishy,]

# Computing the values one by one
means <- data.frame(lapply(val, mean))
names(means) <- paste(names(means), "mean", sep = "-") # Renaming

medians <- data.frame(lapply(val, median))
names(medians) <-
  paste(names(medians), "median", sep = "-") # Renaming

standDevs <- data.frame(lapply(val, sd))
names(standDevs) <-
  paste(names(standDevs), "std", sep = "-") # Renaming

mins <- data.frame(lapply(val, min))
names(mins) <- paste(names(mins), "min", sep = "-") # Renaming

maxs <- data.frame(lapply(val, max))
names(maxs) <- paste(names(maxs), "max", sep = "-") # Renaming

## Ex 1.2
# - The mean wage is given by 5465.321 which is our best bet at predicting the
#   wage without any further anylisis (e.g regression)
# - We can however observe big differences in wage (very low minimum and high
#   standard deviation)
# - This suggest examples with wages far from the mean are common
# - The minimum wage given as 41.8 is suspiciously low looking at the mean and 
#   std of the whole population (outlier?)

## Ex 1.3

# Seperating the data according to gender
genderIdx <- val[, names(val) == "Gender"] > 1

# Only selecting education and wage
gender1 <- val[genderIdx, names(val) == "Education" |
                 names(val) == "Wage"]
gender2 <-
  val[!genderIdx, names(val) == "Education" | names(val) == "Wage"]

# Computing statistics seperately for both genders
g1mean <- data.frame(lapply(gender1, mean))
g2mean <- data.frame(lapply(gender2, mean))

g1median <- data.frame(lapply(gender1, median))
g2median <- data.frame(lapply(gender2, median))

g1min <- data.frame(lapply(gender1, min))
g2min <- data.frame(lapply(gender2, min))

g1max <- data.frame(lapply(gender1, max))
g2max <- data.frame(lapply(gender2, max))

g1sd <- data.frame(lapply(gender1, sd))
g2sd <- data.frame(lapply(gender2, sd))

# - We observe that g1mean = 5071.074 < 5730.35 = g2mean
# - We can also observe that the mean education-duration is roughly the same 
#   with 14.1194 vs 14.26087
# - This suggests that the wage of gender 1 is lower than that of gender 2 and 
#   that this difference does not result from different education-durations.
# Whether this difference in wage-mean is significant can be decided with a 
# 2-sample test:

t.test(gender1[, names(gender1) == "Wage"], gender2[, names(gender2) == "Wage"])

# Result: 95 percent confidence interval for the true difference in mean = 
# [-871.9073 -446.6451]
# => For the true means we conclude that g1mean < g2mean at a significance-level
# of 5%

t.test(gender1[, names(gender1) == "Education"], gender2[, names(gender2) ==
                                                           "Education"])

# Confirms that mean education-duration is roughly the same (95 percent 
# confidence interval of difference: -0.7202883  0.4373551)
